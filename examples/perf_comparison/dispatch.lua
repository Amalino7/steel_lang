local Square = {}
function Square.new(side)
    return setmetatable({side = side}, {__index = Square})
end
function Square:area()
    return self.side * self.side
end

local Circle = {}
function Circle.new(radius)
    return setmetatable({radius = radius}, {__index = Circle})
end
function Circle:area()
    return 3.14159 * self.radius * self.radius
end

local function get_area(s)
    return s:area()
end

local sq = Square.new(10)
local circ = Circle.new(5)

local i = 0
local total = 0
while i < 1000000 do
    if i % 2 == 0 then
        total = total + get_area(sq)
    else
        total = total + get_area(circ)
    end
    i = i + 1
end
print(total)
