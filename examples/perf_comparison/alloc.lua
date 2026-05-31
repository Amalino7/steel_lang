local function Point(x, y, z)
    return {x = x, y = y, z = z}
end

local i = 0
while i < 1000000 do
    local p = Point(i, i + 1, i + 2)
    i = i + 1
end
print(i)
