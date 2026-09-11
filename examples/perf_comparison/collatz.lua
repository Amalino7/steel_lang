function collatz_steps(n)
    local steps = 0
    local curr = n
    while curr ~= 1 do
        if curr % 2 == 0 then
            curr = curr / 2
        else
            curr = curr * 3 + 1
        end
        steps = steps + 1
    end
    return steps
end

local i = 1
local max_steps = 0
while i < 50000 do
    local s = collatz_steps(i)
    if s > max_steps then
        max_steps = s
    end
    i = i + 1
end
print(max_steps)
