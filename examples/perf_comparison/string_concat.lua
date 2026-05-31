local s = ""
local i = 0
while i < 50000 do
    s = s .. "a"
    i = i + 1
end
print(#s)
