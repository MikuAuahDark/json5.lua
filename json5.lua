-- json5.lua: JSON5 parser written in pure Lua.
--
-- Copyright (c) 2024 Miku AuahDark
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy of
-- this software and associated documentation files (the "Software"), to deal in
-- the Software without restriction, including without limitation the rights to
-- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is furnished to do
-- so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.


local whitespace = {
	"\225\154\128",
	"\226\128\128",
	"\226\128\129",
	"\226\128\130",
	"\226\128\131",
	"\226\128\132",
	"\226\128\133",
	"\226\128\134",
	"\226\128\135",
	"\226\128\136",
	"\226\128\137",
	"\226\128\138",
	"\226\128\168",
	"\226\128\169",
	"\226\128\175",
	"\226\129\159",
	"\227\128\128",
	"\194\160", -- &nbsp;
	"\r\n", -- CRLF
	"\9", -- Tab
	"\10", -- Newline
	"\11", -- Vert. tab
	"\12", -- Form feed
	"\13", -- CR
	" ",
}

local newlineChars = {
	"\226\128\168",
	"\226\128\169",
	"\r\n", -- CRLF
	"\r", -- CR
	"\n", -- LF
}

local escaper = {
	["0"] = "\0",
	["'"] = "'",
	["\""] = "\"",
	["\\"] = "\\",
	b = "\8",
	f = "\12",
	n = "\n",
	r = "\r",
	t = "\t",
	v = "\11"
}

local function Q(obj)
	return (string.format("%q", obj):gsub("\r", "\\r"):gsub("\n", "\\n"))
end

---@param text string
local function getWhitespace(text)
	for _, w in ipairs(whitespace) do
		if text:sub(1, #w) == w then
			return w
		end
	end

	return nil
end

---@param text string
local function getNewline(text)
	for _, nl in ipairs(newlineChars) do
		if text:sub(1, #nl) == nl then
			return nl
		end
	end

	return nil
end

---@param text string
local function stripWhitespace(text)
	local newlines = 0

	-- This is quite expensive, O(n^2)
	while true do
		local ws = getWhitespace(text)

		if ws == nil then
			break
		elseif getNewline(ws) then
			newlines = newlines + 1
		end

		text = text:sub(#ws + 1)
	end

	return text, newlines
end

---@param text string
local function stripInlineComments(text)
	-- Find newline
	while true do
		if #text == 0 then
			-- EOF
			return "", 0
		end

		local nl = getNewline(text)
		if nl then
			text = text:sub(#nl + 1)
			break
		end

		text = text:sub(2)
	end

	return text, 1
end

---@param text string
---@return string|nil
---@return integer|string
local function stripBlockComments(text)
	-- Find block comment close tag
	local stopComment = text:find("*/", 1, true)
	if not stopComment then
		return nil, "missing multiline comment close tag"
	end

	local newlines = select(2, text:sub(1, stopComment):gsub("\n", ""))
	return text:sub(stopComment + 2), newlines
end

---Copied and modified slightly from rxi/json
---@param n integer
local function codepointToutf8(n)
	-- http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=iws-appendixa
	if n <= 0x7f then
		return string.char(n)
	elseif n <= 0x7ff then
		return string.char(math.floor(n / 64) + 192, n % 64 + 128)
	elseif n <= 0xffff then
		return string.char(math.floor(n / 4096) + 224, math.floor(n % 4096 / 64) + 128, n % 64 + 128)
	elseif n <= 0x10ffff then
		return string.char(
		math.floor(n / 262144) + 240,
		math.floor(n % 262144 / 4096) + 128,
		math.floor(n % 4096 / 64) + 128,
		n % 64 + 128
	)
	end

	return nil, string.format("invalid unicode codepoint '%x'", n)
end

---@param text string includes the "\u"
local function parseUnicodeImpl(text)
	local hexcode = text:match("\\u(%x%x%x%x)")
	if not hexcode then
		return nil, "invalid unicode hex escape sequence"
	end

	local utf16code = tonumber(hexcode, 16)
	if not utf16code then
		return nil, "invalid unicode hex escape sequence"
	end

	return utf16code, text:sub(7)
end

---@param low integer
---@param high integer
local function getSurrogatePair(low, high)
	return (high - 0xd800) * 0x400 + (low - 0xdc00) + 0x10000
end

---@param text string includes the "\u"
---@return string|nil
---@return string
local function parseUnicode(text)
	local num, msg = parseUnicodeImpl(text)
	if not num then
		return nil, msg
	end

	text = msg

	-- Is it surrogate pair?
	if num >= 0xd800 and num < 0xdc00 then
		-- High surrogate pair. Need low surrogate pair.
		local lownum
		lownum, msg = parseUnicodeImpl(text)
		if lownum and lownum >= 0xdc00 and lownum <= 0xdfff then
			num = getSurrogatePair(lownum, num)
			text = msg
		end
		-- TODO: Should we error in case of invalid pairs?
	end

	local utf8text, errmsg = codepointToutf8(num)
	if utf8text then
		return utf8text, text
	end

	---@cast errmsg -nil
	return nil, errmsg
end

---@param text string
---@param stop fun(text:string):boolean
---@param identifierMode boolean
local function parseStringImpl(text, stop, identifierMode)
	local result = {}
	local newlines = 0

	while true do
		if stop(text) then
			break
		end

		local char = text:sub(1, 1)

		if char == "\\" then
			local what = text:sub(2, 2)

			if escaper[what] then
				if identifierMode then
					return false, "escape sequence not allowed", newlines, text
				end

				result[#result+1] = escaper[what]
				text = text:sub(3)
			elseif what == "u" then
				-- Unicode escape
				local unicode, err = parseUnicode(text)
				if not unicode then
					return false, err, newlines, text
				end

				result[#result+1] = unicode
				text = err -- "\uHHHH" optionally followed by 1 another "\uHHHH" for surrogate pair
			elseif what == "x" then
				if identifierMode then
					return false, "hex escape sequence not allowed", newlines, text
				end

				local hexstr = text:sub(2, 3)
				local hexnum = tonumber(hexstr, 16)
				if not hexnum then
					return false, "invalid hex escape sequence", newlines, text
				end

				result[#result+1] = string.char(hexnum)
				text = text:sub(5) -- "\xHH"
			else
				if identifierMode then
					return false, "invalid escape sequence", newlines, text
				end

				local nl = getNewline(text:sub(2))
				local ignore = 2
				if nl then
					-- JSON5 allows string spanning multiple lines by escaping newline
					newlines = newlines + 1
					ignore = #nl + 2
				end

				-- Ignore
				text = text:sub(ignore)
			end
		elseif char:byte(1, 1) < 32 then
			return false, "control character found", newlines, text
		else
			result[#result+1] = char
			text = text:sub(2)
		end
	end

	return true, table.concat(result), newlines, text
end

---@param text string including the delimiter
local function parseString(text)
	local stop = text:sub(1, 1)

	local success, value, newlines, newText = parseStringImpl(
		text:sub(2),
		function(txt) return txt:sub(1, 1) == stop end,
		false
	)

	if success then
		newText = newText:sub(2)
	end

	return success, value, newlines, newText
end

local nan = 0/0

---@param text string
local function parseNumber(text)
	local sign = 1
	local signchar = text:sub(1, 1)

	if signchar == "+" then
		sign = 1
		text = text:sub(2)
	elseif signchar == "-" then
		sign = -1
		text = text:sub(2)
	end

	if text:sub(1, 3) == "NaN" then
		return true, nan, 0, text:sub(4)
	end

	local infText = text:find("Infinity", 1, true)
	if infText == 1 then
		return true, math.huge * sign, 0, text:sub(9)
	end

	-- TODO: Bring our own number parsing for Lua 5.1?
	local potentialNum = 0
	local lookText = text

	while true do
		if getWhitespace(lookText) then
			break
		end

		if lookText:sub(1, 1) == "," then
			break
		end

		potentialNum = potentialNum + 1
		lookText = lookText:sub(2)
	end

	local numval = text:sub(1, potentialNum)
	local num = nil

	if numval:sub(1, 1) == "0" and numval:sub(2):find("^%d+$") then
		-- Octal is not allowed
		num = nil
	else
		-- Parse normal
		num = tonumber(numval)
	end
	if num == nil then
		return false, "invalid number sequence "..Q(numval), 0, text
	end

	return true, num * sign, 0, text:sub(potentialNum + 1)
end

---@param text string
---@param nullval any
local function parseNull(text, nullval)
	if text:sub(1, 4) ~= "null" then
		return false, "invalid null literal", 0, text
	end

	return true, nullval, 0, text:sub(5)
end

---@param text string
local function parseBoolean(text)
	if text:sub(1, 4) == "true" then
		return true, true, 0, text:sub(5)
	elseif text:sub(1, 5) == "false" then
		return true, false, 0, text:sub(6)
	else
		return false, "invalid boolean literal", 0, text
	end
end

---@param text string
local function stripComments(text)
	local s = text:sub(1, 2)
	if s == "//" then
		return stripInlineComments(text:sub(3))
	elseif s == "/*" then
		return stripBlockComments(text:sub(3))
	end

	return text, 0
end

---@param text string
local function stripWhitespaceAndComments(text)
	local newlines = 0

	while true do
		local txt, nl = stripWhitespace(text)
		local txt2, nl2 = stripComments(txt)

		if not txt2 then
			---@cast nl2 string
			return false, nl2, newlines + nl
		end

		newlines = newlines + nl + nl2
		if txt2 == text then
			break
		end

		text = txt2
	end

	return true, text, newlines
end

local parseValue

---@param text string
---@param nullval any
---@return boolean,any[]|string,integer,string
local function parseArray(text, nullval)
	text = text:sub(2)

	local result = {}
	local newlines = 0

	while true do
		local success, newText, nl = stripWhitespaceAndComments(text)
		if not success then
			return false, newText, newlines + nl, text
		end

		newlines = newlines + nl

		if newText:sub(1, 1) == "]" then
			-- Finish
			text = newText:sub(2)
			break
		end

		local value
		success, value, nl, newText = parseValue(newText, nullval)
		newlines = newlines + nl

		if not success then
			return false, value, newlines, newText
		end

		local newText2
		success, newText2, nl = stripWhitespaceAndComments(newText)
		newlines = newlines + nl
		if not success then
			return false, newText2, newlines, newText
		end

		-- Insert
		result[#result+1] = value

		-- Continue or finish?
		local lastOrNext = newText2:sub(1, 1)
		text = newText2:sub(2)

		if lastOrNext == "]" then
			-- Finish
			break
		elseif lastOrNext ~= "," then
			return false, "expected comma got \""..lastOrNext.."\"", newlines, newText2
		end
	end

	return true, result, newlines, text
end

---@param identifier string
local function testIdentifier(identifier)
	local firstID = identifier:byte(1, 1)
	if firstID >= 48 and firstID <= 57 then
		return false
	end

	for i = 1, #identifier do
		local char = identifier:byte(i, i)
		if char < 36 then
			return false
		end

		if char >= 37 and char <= 47 then
			return false
		end

		if char >= 58 and char <= 64 then
			return false
		end

		if char >= 91 and char <= 94 then
			return false
		end

		if char == 96 then
			return false
		end

		if char >= 123 and char <= 128 then
			return false
		end
	end

	return true
end

---@param text string
local function stopIdentifier(text)
	return text:sub(1, 1) == ":" or getWhitespace(text) ~= nil
end

---@param text string
local function parseIdentifier(text)
	local first = text:sub(1, 1)
	local identifier, newText

	if first == "'" or first == "\"" then
		-- Quoted identifier
		local success, _
		success, identifier, _, newText = parseStringImpl(
			text:sub(2),
			function (txt) return txt:sub(1, 1) == first end,
			true
		)
		if not success then
			return false, identifier, newText
		end

		newText = newText:sub(2)
	else
		-- Unquoted identifier
		local success, _
		success, identifier, _, newText = parseStringImpl(text, stopIdentifier, true)
		if not success then
			return false, identifier, newText
		end

		-- Test identifier validity
		if not testIdentifier(identifier) then
			return false, "invalid identifier "..Q(identifier), newText
		end
	end

	return true, identifier, newText
end

---@param text string
---@param nullval any
---@return boolean,table<string,any>|string,integer,string
local function parseObject(text, nullval)
	text = text:sub(2)

	local result = {}
	local newlines = 0

	while true do
		local success, newText, nl = stripWhitespaceAndComments(text)
		newlines = newlines + nl
		if not success then
			return false, newText, newlines, text
		end

		if newText:sub(1, 1) == "}" then
			-- Finish
			text = newText:sub(2)
			break
		end

		-- Identifier
		local identifier
		success, identifier, newText = parseIdentifier(newText)
		if not success then
			return false, identifier, newlines, newText
		end

		local newText2
		success, newText2, nl = stripWhitespaceAndComments(newText)
		newlines = newlines + nl
		if not success then
			return false, newText2, newlines, newText
		end

		if newText2:sub(1, 1) ~= ":" then
			return false, "expected colon after identifier, got "..Q(newText2:sub(1, 1)), newlines, newText
		end

		newText = newText2

		-- Value
		success, newText2, nl = stripWhitespaceAndComments(newText)
		newlines = newlines + nl
		if not success then
			return false, newText2, newlines, newText
		end
		newText = newText2:sub(2)

		local value
		success, value, nl, newText = parseValue(newText, nullval)
		newlines = newlines + nl

		if not success then
			return false, value, newlines, newText
		end

		success, newText2, nl = stripWhitespaceAndComments(newText)
		newlines = newlines + nl
		if not success then
			return false, newText2, newlines, newText
		end

		-- Insert
		result[identifier] = value

		-- Continue or finish?
		local lastOrNext = newText2:sub(1, 1)
		text = newText2:sub(2)

		if lastOrNext == "}" then
			-- Finish
			break
		elseif lastOrNext ~= "," then
			return false, "expected comma got \""..lastOrNext.."\"", newlines, newText2
		end
	end

	return true, result, newlines, text
end

---@param text string
local function catchEOF(text)
	return false, "unexpected eof", 0, text
end

local valueTest = {
	["-"] = parseNumber,
	["+"] = parseNumber,
	["."] = parseNumber,
	["0"] = parseNumber,
	["1"] = parseNumber,
	["2"] = parseNumber,
	["3"] = parseNumber,
	["4"] = parseNumber,
	["5"] = parseNumber,
	["6"] = parseNumber,
	["7"] = parseNumber,
	["8"] = parseNumber,
	["9"] = parseNumber,
	["N"] = parseNumber, -- for NaN
	["I"] = parseNumber, -- for Infinity
	["n"] = parseNull,
	["t"] = parseBoolean,
	["f"] = parseBoolean,
	["'"] = parseString,
	["\""] = parseString,
	["["] = parseArray,
	["{"] = parseObject,
	[""] = catchEOF,
}

---@param text string
function parseValue(text, nullval)
	local success, newText, newlines = stripWhitespaceAndComments(text)
	if not success then
		return false, newText, newlines, text
	end

	local first = newText:sub(1, 1)
	local func = valueTest[first]

	if not func then
		return false, "invalid value literal"..Q(first), newlines, newText
	end

	local value, newlines2
	success, value, newlines2, newText = func(newText, nullval)
	return success, value, newlines + newlines2, newText
end

---@param message string
---@param fulltext string
---@param subtext string
---@param newlines integer
local function formatError(message, fulltext, subtext, newlines)
	local diff = fulltext:sub(1, #fulltext - #subtext)
	local gotnewline = 0

	while gotnewline < newlines do
		local nl = diff:find("\n", 1, true)
		if not nl then
			break
		end

		gotnewline = gotnewline + 1
		diff = diff:sub(nl + 1)
	end

	local errstr = string.format("%s at line %d col %d", message, gotnewline + 1, #diff + 1)
	error(errstr, 2)
end

local json5 = {}

---A value that denote "null" value in JSON, if the user need to preserve keys.
---@type any
json5.null = newproxy(false)

---@class json5.opts
---@field public null any Null value substitute. Default is `nil` which means "null" will not preserve key with "null" value and potentially leave holes in an array. To preserve "null", use `json5.null` or any other value.

---Decode JSON5 string to Lua value.
---@param text string JSON5 string.
---@param opts json5.opts? Additional option to specify. See `json5.opts` for more information.
---@return any
function json5.decode(text, opts)
	local nullval = nil
	if opts then
		nullval = opts.null
	end

	local success, newText, newlines = stripWhitespaceAndComments(text)
	if not success then
		formatError(newText, text, text, newlines)
	end

	local value, newlines2, newText2
	success, value, newlines2, newText2 = parseValue(text, nullval)
	newlines = newlines + newlines2
	if not success then
		---@cast value string
		formatError(value, text, newText, newlines)
	end
	newText = newText2

	success, newText2, newlines2 = stripWhitespaceAndComments(newText)
	newlines = newlines + newlines2
	if not success then
		formatError(newText2, text, newText, newlines)
	end

	if #newText2 > 0 then
		formatError("trailing garbage", text, newText2, newlines)
	end

	return value
end

return json5
