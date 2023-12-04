from hashlib import md5

s0 = "iwrupvqb"
for i in range(1000000):
    s = s0 + str(i)
    hashed = md5(s.encode()).hexdigest()
    if hashed.startswith("00000"):
        print(i, s)
        break
