local l = {}
local i = 0
while i < 1000000 do
    table.insert(l, i)
    i = i + 1
end
print(#l)
