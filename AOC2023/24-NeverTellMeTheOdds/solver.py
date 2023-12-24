from z3 import Int, Solver, solve

# with open('./eqex.txt', 'r') as f:
with open('./eq.txt', 'r') as f:
    lines = f.read().splitlines()
eqstr = [l.replace('- -', '+ ') for l in lines]
print(eqstr)

x = Int('x')
y = Int('y')
z = Int('z')
u = Int('u')
v = Int('v')
w = Int('w')
t0 = Int('t0')
t1 = Int('t1')
t2 = Int('t2')

s = Solver()
for st in eqstr: 
    print(st)
    s.add(eval(st + ' == 0'))
print(s)
print(s.check())
solution = s.model()
print(solution)
print(solution[x], solution[y], solution[z])
r = Int('r')
solve(r == solution[x] + solution[y]+solution[z])
