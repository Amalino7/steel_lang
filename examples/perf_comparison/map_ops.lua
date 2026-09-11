local m = {}
local i = 0
while i < 1000000 do
    m[i] = i
    i = i + 1
end

local count = 0
for _ in pairs(m) do count = count + 1 end
print(count)
