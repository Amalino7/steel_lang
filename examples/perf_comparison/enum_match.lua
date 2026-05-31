local function handle(s)
    if s.tag == "Ok" then
        return 1
    elseif s.tag == "Error" then
        return 0
    elseif s.tag == "Pending" then
        return 2
    end
end

local s1 = {tag = "Ok"}
local s2 = {tag = "Error", msg = "fail"}
local s3 = {tag = "Pending"}

local i = 0
local total = 0
while i < 1000000 do
    if i % 3 == 0 then
        total = total + handle(s1)
    elseif i % 3 == 1 then
        total = total + handle(s2)
    else
        total = total + handle(s3)
    end
    i = i + 1
end
print(total)
