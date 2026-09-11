local function make_adder(x)
    return function(y)
        return x + y
    end
end

local adder = make_adder(10)
local i = 0
local total = 0
while i < 1000000 do
    total = total + adder(i)
    i = i + 1
end
print(total)
