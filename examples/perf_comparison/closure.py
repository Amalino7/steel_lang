def make_adder(x):
    def adder(y):
        return x + y
    return adder

adder = make_adder(10)
i = 0
total = 0
while i < 1000000:
    total += adder(i)
    i += 1
print(total)
