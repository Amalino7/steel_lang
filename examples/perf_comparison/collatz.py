def collatz_steps(n):
    steps = 0
    curr = n
    while curr != 1:
        if curr % 2 == 0:
            curr = curr // 2
        else:
            curr = curr * 3 + 1
        steps += 1
    return steps

i = 1
max_steps = 0
while i < 50000:
    s = collatz_steps(i)
    if s > max_steps:
        max_steps = s
    i += 1
print(max_steps)
