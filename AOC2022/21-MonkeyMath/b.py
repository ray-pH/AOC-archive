opf = {
    '+' : lambda x, y: x + y,
    '-' : lambda x, y: x - y,
    '*' : lambda x, y: x * y,
    '/' : lambda x, y: x / y,
}
# a = X . b
# X = a ~ b
def left_invop(tup, a):
    (_, op, b) = tup
    if op == '+': return (a, '-', b)
    if op == '-': return (a, '+', b)
    if op == '*': return (a, '/', b)
    if op == '/': return (a, '*', b)

# a = b . X
# X = a ~ b
def right_invop(tup, a):
    (b, op, _) = tup
    if op == '+': return (a, '-', b)
    if op == '-': return (b, '-', a)
    if op == '*': return (a, '/', b)
    if op == '/': return (b, '/', a)


def parse_line(line : str):
    name, rest = line.split(':')
    if name == 'humn': return
    try:
        value = int(rest)
        valuedict[name] = value
    except ValueError:
        x1, op, x2 = rest.strip().split(" ")
        # for now, only handle the case where 'root' yell sum
        if name == 'root' and op == '+':
            # just a hack so that when we invert the operation,
            # and set 'root' to be 0, we get x1 = x2
            monkeydict[name] = (x1, '-', x2)
        else:
            monkeydict[name] = (x1, op, x2)

def reverse(name):
    for k, v in monkeydict.items():
        (x1, _, x2) = v
        if not (x1 == name or x2 == name): continue
        monkeydict.pop(k)
        if x1 == name:
            monkeydict[x1] = left_invop(v, k)
        if x2 == name:
            monkeydict[x2] = right_invop(v, k)
        if k != 'root':
            reverse(k)
        return

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

valuedict = {'root': 0}
monkeydict = {}
for l in lines:
    parse_line(l)
reverse('humn')
print(monkeydict)

print(getvalue('humn'))
