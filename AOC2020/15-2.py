ex1 = [0, 3, 6]
inpu = [17,1,3,16,19,0]
inp = inpu
pdic = {}
dic = {}
for i in range(len(inp)):
    pdic = dic.copy()
    dic.update( {inp[i] : i+1} )

n = len(inp)
val = inp[-1]
preval = inp[-1]
lim = 2020
lim = 30000000
for i in range(lim - len(inp)):
    n += 1
    if val in pdic : 
        val = n - pdic[val] - 1
    else : 
        val = 0
        print(n, ':',  val)
    pdic.update({preval : n-1})
    dic.update({val : n})
    preval = val

print(len(dic))
print(val)
