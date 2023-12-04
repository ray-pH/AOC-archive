def parsefile(lines):
    emptyline_id = 0
    for i, line in enumerate(lines):
        if line == '':
            emptyline_id = i
            break
    map = lines[:emptyline_id]
    pathstr = lines[emptyline_id+1:][0]
    return map, parsepath(pathstr)

def parsepath(pathstr : str):
    pathstr = " R ".join(pathstr.split('R'))
    pathstr = " L ".join(pathstr.split('L'))
    return pathstr.split(' ')

def get_initial_pos(map):
    for c, char in enumerate(map[0]):
        if char == '.':
            return (0, c)
    return (0,0)

def wrappos(map, pos):
    row, col = pos
    if row < 0 : row = len(map)-1
    if row >= len(map): row = 0
    if col < 0 : col = len(map[row])-1
    if col >= len(map[row]): col = 0
    return (row, col)

def get_front(map, pos, dir):
    frontpos = pos
    frontpos = wrappos(map, addvec(frontpos, dir))
    frontchar = map[frontpos[0]][frontpos[1]]
    while frontchar == ' ':
        frontpos = wrappos(map, addvec(frontpos, dir))
        frontchar = map[frontpos[0]][frontpos[1]]
    return frontpos, frontchar

def move(map, pos, dir, n):
    if n == 0: return pos
    frontpos, frontchar = get_front(map, pos, dir)
    if frontchar == '#': return pos
    return move(map, frontpos, dir, n-1)

dirvec = [
    (0, 1), # right
    (1, 0), # down
    (0, -1), # left
    (-1, 0), # up
]
addvec   = lambda a, b : (a[0]+b[0], a[1]+b[1])
scalevec = lambda s, v : (s*v[0], s*v[1])
turnR = lambda d : (d+1) % 4
turnL = lambda d : (d-1) % 4
# ------------------------------------------------

# with open('inpex.txt', 'r') as f:
with open('input.txt', 'r') as f:
    lines = f.read().splitlines()
map, path = parsefile(lines)

mapcols = max([len(row) for row in map])
map = [row + ' '*(mapcols-len(row)) for row in map]

print(path)

dir = 0
pos = get_initial_pos(map)
print(pos)

for i,p in enumerate(path):
    if i%2 == 0:
        # number of steps
        n = int(p)
        pos = move(map, pos, dirvec[dir], n)
    else:
        # turn
        if p == 'R': dir = turnR(dir)
        elif p == 'L': dir = turnL(dir)
        else: assert False, 'invalid turn'
print(pos)

row = pos[0]+1
col = pos[1]+1
passw = 1000*row + 4*col + dir
print(passw)
