f = open("3-input.txt", "r")
val = f.read()
f.close()

arr = val.split('\n')
print(arr)

w = len(arr[0])
h = len(arr)

def howm(ix,iy):
    x, y = 0,0
    n = 0
    while y < h-2:
        x = (x+ix)%w
        y += iy
        c = arr[y][x]
        if c == '#' : n+= 1
    return n

print(howm(1,1), howm(3,1), howm(5,1), howm(7,1), howm(1,2))
