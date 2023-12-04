# with open('input.txt') as f:
with open('inpex2.txt') as f:
    lines = f.readlines()

lines = ['.'+x.strip()+'.' for x in lines]
length = len(lines[0])
schematics = ['.'*length] + lines + ['.'*length]

partnumbers = []
currnumberstr = ""
for r, row in enumerate(schematics):
    for c, char in enumerate(row):
        if char.isnumeric():
            currnumberstr += char
            continue
        if currnumberstr:
            data = (r,c-len(currnumberstr),int(currnumberstr))
            partnumbers.append(data)
            currnumberstr = ""

def neighbors(r,chead,number):
    l = len(str(number))
    cleft = chead - 1
    cright = chead + l
    ctop = zip([r-1]*(l+2),range(cleft,cright+1))
    cbot = zip([r+1]*(l+2),range(cleft,cright+1))
    cmid = [(r,cleft), (r, cright)]
    return list(ctop) + list(cbot) + cmid

def is_symbol(c):
    return c != '.' and not c.isnumeric()

def is_valid_pn(pndata, schematics):
    r, chead, string = pndata
    for r,c in neighbors(r,chead,string):
        if is_symbol(schematics[r][c]):
            return True
    return False

sum = 0
for pndata in partnumbers:
    if is_valid_pn(pndata, schematics):
        sum += pndata[2]
print(sum)
