json5.lua
=====

JSON5 parser written in pure Lua.

Unlike [lua-json5](https://github.com/Joakker/lua-json5), this JSON5 parser runs without any dependencies and works
in Lua 5.1 or later.

Although, unlike lua-json5, this JSON5 only performs decoding. Since generally JSON5 is used for configuration,
having function to encode to JSON5 is not very useful. In general, a valid JSON is also a valid JSON5.

Installation
-----

Simply drop `json5.lua` into your project.

License
-----

MIT
