--[[
Copyright (c) 2010-2013 Matthias Richter

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

---@class Timer
local Timer = {}
Timer.__index = Timer

---@alias TimedHandle table

local function _nothing_() end

local function updateTimerHandle(handle, dt)
		-- handle: {
		--   time = <number>,
		--   after = <function>,
		--   during = <function>,
		--   limit = <number>,
		--   count = <number>,
		-- }
		handle.time = handle.time + dt
		handle.during(dt, math.max(handle.limit - handle.time, 0))

		while handle.time >= handle.limit and handle.count > 0 do
			if handle.after(handle.after) == false then
				handle.count = 0
				break
			end
			handle.time = handle.time - handle.limit
			handle.count = handle.count - 1
		end
end

---@param dt number Elapsed seconds.
function Timer:update(dt)
	-- timers may create new timers, which leads to undefined behavior
	-- in pairs() - so we need to put them in a different table first
	local to_update = {}
	for handle in pairs(self.functions) do
		to_update[handle] = handle
	end

	for handle in pairs(to_update) do
		if self.functions[handle] then
			updateTimerHandle(handle, dt)
			if handle.count == 0 then
				self.functions[handle] = nil
			end
		end
	end
end

---@param delay number How long to wait, in seconds.
---@param during function The function to call every frame.
---@param after function? The function to call afterward.
---@return TimedHandle
function Timer:during(delay, during, after)
	local handle = { time = 0, during = during, after = after or _nothing_, limit = delay, count = 1 }
	self.functions[handle] = true
	return handle
end

---@param delay number How long to wait, in seconds.
---@param func function The function to call afterward.
---@return TimedHandle
function Timer:after(delay, func)
	return self:during(delay, _nothing_, func)
end

---@param delay number How long to wait, in seconds.
---@param after function The function to call over and over.
---@param count number? How many times to repeat.
---@return TimedHandle
function Timer:every(delay, after, count)
	local count = count or math.huge -- exploit below: math.huge - 1 = math.huge
	local handle = { time = 0, during = _nothing_, after = after, limit = delay, count = count }
	self.functions[handle] = true
	return handle
end

---@param handle TimedHandle Stops a timed function.
function Timer:cancel(handle)
	self.functions[handle] = nil
end

function Timer:clear()
	self.functions = {}
end

---@param f fun(wait: fun(seconds: number)) Function to execute in parallel.
function Timer:script(f)
	local co = coroutine.wrap(f)
	co(function(t)
		self:after(t, co)
		coroutine.yield()
	end)
end

---@class TweenObject
Timer.tween = {
	-- helper functions
	---@param f function
	out = function(f) -- 'rotates' a function
		return function(s, ...) return 1 - f(1-s, ...) end
	end,
	---@param f1 function
	---@param f2 function
	chain = function(f1, f2) -- concatenates two functions
		return function(s, ...) return (s < .5 and f1(2*s, ...) or 1 + f2(2*s-1, ...)) * .5 end
	end,

	-- useful tweening functions
	---@param s number
	linear = function(s) return s end,
	---@param s number
	quad   = function(s) return s*s end,
	---@param s number
	cubic  = function(s) return s*s*s end,
	---@param s number
	quart  = function(s) return s*s*s*s end,
	---@param s number
	quint  = function(s) return s*s*s*s*s end,
	---@param s number
	sine   = function(s) return 1-math.cos(s*math.pi/2) end,
	---@param s number
	expo   = function(s) return 2^(10*(s-1)) end,
	---@param s number
	circ   = function(s) return 1 - math.sqrt(1-s*s) end,

	---@param s number
	---@param bounciness number
	back = function(s,bounciness)
		bounciness = bounciness or 1.70158
		return s*s*((bounciness+1)*s - bounciness)
	end,

	---@param s number
	bounce = function(s) -- magic numbers ahead
		local a,b = 7.5625, 1/2.75
		return math.min(a*s^2, a*(s-1.5*b)^2 + .75, a*(s-2.25*b)^2 + .9375, a*(s-2.625*b)^2 + .984375)
	end,

	---@param s number
	---@param amp number
	---@param period number
	elastic = function(s, amp, period)
		amp, period = amp and math.max(1, amp) or 1, period or .3
		return (-amp * math.sin(2*math.pi/period * (s-1) - math.asin(1/amp))) * 2^(10*(s-1))
	end,
}
setmetatable(Timer.tween, {
-- register new tween
__call = function(tween, self, len, subject, target, method, after, ...)
	-- recursively collects fields that are defined in both subject and target into a flat list
	local function tween_collect_payload(subject, target, out)
		for k,v in pairs(target) do
			local ref = subject[k]
			assert(type(v) == type(ref), 'Type mismatch in field "'..k..'".')
			if type(v) == 'table' then
				tween_collect_payload(ref, v, out)
			else
				local ok, delta = pcall(function() return (v-ref)*1 end)
				assert(ok, 'Field "'..k..'" does not support arithmetic operations')
				out[#out+1] = {subject, k, delta}
			end
		end
		return out
	end

	method = tween[method or 'linear'] -- see __index
	local payload, t, args = tween_collect_payload(subject, target, {}), 0, {...}

	local last_s = 0
	return self:during(len, function(dt)
		t = t + dt
		local s = method(math.min(1, t/len), unpack(args))
		local ds = s - last_s
		last_s = s
		for _, info in ipairs(payload) do
			local ref, key, delta = unpack(info)
			ref[key] = ref[key] + delta * ds
		end
	end, after)
end,

-- fetches function and generated compositions for method `key`
__index = function(tweens, key)
	if type(key) == 'function' then return key end

	assert(type(key) == 'string', 'Method must be function or string.')
	if rawget(tweens, key) then return rawget(tweens, key) end

	local function construct(pattern, f)
		local method = rawget(tweens, key:match(pattern))
		if method then return f(method) end
		return nil
	end

	local out, chain = rawget(tweens,'out'), rawget(tweens,'chain')
	return construct('^in%-([^-]+)$', function(...) return ... end)
	       or construct('^out%-([^-]+)$', out)
	       or construct('^in%-out%-([^-]+)$', function(f) return chain(f, out(f)) end)
	       or construct('^out%-in%-([^-]+)$', function(f) return chain(out(f), f) end)
	       or error('Unknown interpolation method: ' .. key)
end})

-- Timer instancing
---@return Timer
function Timer.new()
	return setmetatable({functions = {}, tween = Timer.tween}, Timer)
end

-- default instance
local default = Timer.new()

-- module forwards calls to default instance
---@class TimerModule
---@field tween TweenObject
local module = {}
for k in pairs(Timer) do
	if k ~= "__index" then
		module[k] = function(...) return default[k](default, ...) end
	end
end

function module.new()
	return Timer.new()
end

---@param dt number Elapsed seconds.
---@see Timer.update
function module.update(dt)
	default:update(dt)
end

---@param delay number How long to wait, in seconds.
---@param during function The function to call every frame.
---@param after function? The function to call afterward.
---@return TimedHandle
---@see Timer.during
function module.during(delay, during, after)
	return default:during(delay, during, after)
end

---@param delay number How long to wait, in seconds.
---@param func function The function to call afterward.
---@return TimedHandle
---@see Timer.after
function module.after(delay, func)
	return default:during(delay, _nothing_, func)
end

---@param delay number How long to wait, in seconds.
---@param after function The function to call over and over.
---@param count number? How many times to repeat.
---@return TimedHandle
---@see Timer.every
function module.every(delay, after, count)
	return default:every(delay, after, count)
end

---@param handle TimedHandle Stops a timed function.
---@see Timer.cancel
function module.cancel(handle)
	default:cancel(handle)
end

---@see Timer.clear
function module.clear()
	default:clear()
end

---@param f fun(wait: fun(seconds: number)) Function to execute in parallel.
---@see Timer.script
function module.script(f)
	default:script(f)
end

module.tween = setmetatable({}, {
	__index = Timer.tween,
	__newindex = function(k,v) Timer.tween[k] = v end,
	__call = function(t, ...) return default:tween(...) end,
})

setmetatable(module, {__call = Timer.new})

return module
