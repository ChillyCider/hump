--[[
Copyright (c) 2012-2013 Matthias Richter

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

Except as contained in this notice, the name(s) of the above copyright holders
shall not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
]]--

---@class Registry
local Registry = {}
Registry.__index = function(self, key)
	return Registry[key] or (function()
		local t = {}
		rawset(self, key, t)
		return t
	end)()
end

---@param s string Signal name.
---@param f function Signal handler.
---@return function
function Registry:register(s, f)
	self[s][f] = f
	return f
end

---@param s string The signal name.
---@param ... any Arguments to pass to the signal handler.
function Registry:emit(s, ...)
	for f in pairs(self[s]) do
		f(...)
	end
end

---@param s string The signal name.
---@param ... function Handlers to remove.
function Registry:remove(s, ...)
	local f = {...}
	for i = 1,select('#', ...) do
		self[s][f[i]] = nil
	end
end

---@param ... string Names of signals to clear.
function Registry:clear(...)
	local s = {...}
	for i = 1,select('#', ...) do
		self[s[i]] = {}
	end
end

---@param p string Signal name pattern.
---@param ... any Arguments to pass to the signal handlers.
function Registry:emitPattern(p, ...)
	for s in pairs(self) do
		if s:match(p) then self:emit(s, ...) end
	end
end

---@param p string Signal name pattern.
---@param f function Signal handler to register to all matching signals.
function Registry:registerPattern(p, f)
	for s in pairs(self) do
		if s:match(p) then self:register(s, f) end
	end
	return f
end

---@param p string Signal name pattern.
---@param ... function Handlers to remove.
function Registry:removePattern(p, ...)
	for s in pairs(self) do
		if s:match(p) then self:remove(s, ...) end
	end
end

---@param p string Signal name pattern.
function Registry:clearPattern(p)
	for s in pairs(self) do
		if s:match(p) then self[s] = {} end
	end
end

-- instancing
---@return Registry
function Registry.new()
	return setmetatable({}, Registry)
end

-- default instance
local default = Registry.new()

-- module forwards calls to default instance
local module = {}
for k in pairs(Registry) do
	if k ~= "__index" then
		module[k] = function(...) return default[k](default, ...) end
	end
end

function module.new()
	return Registry.new()
end

---@param s string Signal name.
---@param f function Handler.
---@return function
---@see Registry.register
function module.register(s, f)
	return default:register(s, f)
end

---@param s string Signal name.
---@param ... any Arguments to pass to the handlers.
---@see Registry.emit
function module.emit(s, ...)
	default:emit(s, ...)
end

---@param s string Signal name.
---@param ... function Handlers to remove.
---@see Registry.remove
function module.remove(s, ...)
	default:remove(s, ...)
end

---@param s string Signal name.
---@see Registry.clear
function module.clear(s)
	default:clear(s)
end

---@param p string Signal name pattern.
---@param ... any Arguments to pass to the handlers.
---@see Registry.emitPattern
function module.emitPattern(p, ...)
	default:emitPattern(p, ...)
end

---@param p string Signal name pattern.
---@param f function Handler to register to all matching signals.
---@return function
---@see Registry.registerPattern
function module.registerPattern(p, f)
	return default:registerPattern(p, f)
end

---@param p string Signal name pattern.
---@param ... function Handlers to remove.
---@see Registry.removePattern
function module.removePattern(p, ...)
	default:removePattern(p, ...)
end

---@param p string Signal name pattern.
---@see Registry.clearPattern
function module.clearPattern(p)
	default:clearPattern(p)
end

return setmetatable(module, {__call = Registry.new})
