opf = {
    '+' : lambda x, y: x + y,
    '-' : lambda x, y: x - y,
    '*' : lambda x, y: x * y,
    '/' : lambda x, y: x / y,
}

def parse_line(line : str):
    name, rest = line.split(':')
    try:
        value = int(rest)
        valuedict[name] = value
    except ValueError:
        x1, op, x2 = rest.strip().split(" ")
        monkeydict[name] = (x1, op, x2)

def getvalue(name):
    if name in valuedict:
        return valuedict[name]
    else:
        x1, op, x2 = monkeydict[name]
        val = opf[op](getvalue(x1), getvalue(x2))
        valuedict[name] = val
        return val

# --------------------------------------------
# with open('inpex.txt', 'r') as f:
with open('input.txt', 'r') as f:
    lines = f.read().splitlines()

valuedict = {}
monkeydict = {}
for l in lines:
    parse_line(l)

print(getvalue('root'))
