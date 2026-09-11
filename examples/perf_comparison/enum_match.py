class Ok: pass
class Error:
    def __init__(self, msg):
        self.msg = msg
class Pending: pass

def handle(s):
    if isinstance(s, Ok):
        return 1
    elif isinstance(s, Error):
        return 0
    elif isinstance(s, Pending):
        return 2

s1 = Ok()
s2 = Error("fail")
s3 = Pending()

i = 0
total = 0
while i < 1000000:
    if i % 3 == 0:
        total += handle(s1)
    elif i % 3 == 1:
        total += handle(s2)
    else:
        total += handle(s3)
    i += 1
print(total)
