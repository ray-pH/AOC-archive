from hashlib import md5

s0 = "iwrupvqb"
for i in range(100000000):
    if i % 100000 == 0: print(i)
    s = s0 + str(i)
    hashed = md5(s.encode()).hexdigest()
    if hashed.startswith("000000"):
        print(i, s)
        break
