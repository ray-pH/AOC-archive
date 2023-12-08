def parsefile(lines):
    emptyline_id = 0
    for i, line in enumerate(lines):
        if line == '':
            emptyline_id = i
            break
    pathstr = lines[emptyline_id+1:][0]
    mapstr = lines[:emptyline_id]
    return parsemap(mapstr), parsepath(pathstr)

def parsemap(mapstr : str):
    global chunksize
    map = []
    mapmeta = []
    for r in range(0, len(mapstr), chunksize):
        for c in range(0, len(mapstr[r]), chunksize):
            if mapstr[r][c] == ' ': continue
            currentchunk = []
            for i in range(chunksize):
                currentchunk.append(mapstr[r+i][c:c+chunksize])
            map.append(currentchunk)
            mapmeta.append((r,c))
    return map, mapmeta

conn_ex = {
    (1,'L'):(3,'U'), (1,'R'):(6,'R'),
    (1,'U'):(2,'U'), (1,'D'):(4,'U'),
    (2,'L'):(6,'D'), (2,'R'):(3,'L'),
    (2,'U'):(1,'U'), (2,'D'):(5,'D'),
    (3,'L'):(2,'R'), (3,'R'):(4,'L'),
    (3,'U'):(1,'L'), (3,'D'):(5,'L'),
    (4,'L'):(3,'R'), (4,'R'):(6,'U'),
    (4,'U'):(1,'D'), (4,'D'):(5,'U'),
    (5,'L'):(3,'D'), (5,'R'):(6,'L'),
    (5,'U'):(4,'D'), (5,'D'):(2,'D'),
    (6,'L'):(5,'R'), (6,'R'):(1,'R'),
    (6,'U'):(4,'R'), (6,'D'):(2,'L'),
}

conn_inp = {
    (1,'L'):(4,'L'), (1,'R'):(2,'L'),
    (1,'U'):(6,'L'), (1,'D'):(3,'U'),
    (2,'L'):(1,'R'), (2,'R'):(5,'R'),
    (2,'U'):(6,'D'), (2,'D'):(3,'R'),
    (3,'L'):(4,'U'), (3,'R'):(2,'D'),
    (3,'U'):(1,'D'), (3,'D'):(5,'U'),
    (4,'L'):(1,'L'), (4,'R'):(5,'L'),
    (4,'U'):(3,'L'), (4,'D'):(6,'U'),
    (5,'L'):(4,'R'), (5,'R'):(2,'R'),
    (5,'U'):(3,'D'), (5,'D'):(6,'R'),
    (6,'L'):(1,'U'), (6,'R'):(5,'D'),
    (6,'U'):(4,'D'), (6,'D'):(2,'U'),
}
            

def parsepath(pathstr : str):
    pathstr = " R ".join(pathstr.split('R'))
    pathstr = " L ".join(pathstr.split('L'))
    return pathstr.split(' ')

def get_initial_pos(face):
    for c, char in enumerate(face[0]):
        if char == '.': return (0, c)
    return (0,0)

# counterclockwise
def rotate_pos_once(pos : tuple[int,int]) -> tuple[int,int]:
    global chunksize
    (row, col) = pos
    return (col, chunksize-1-row)

def rotate_pos(pos : tuple[int,int], dirfrom : str, dirto : str) -> tuple[int,int]:
    ndirfrom = dirnum[dirfrom]
    ndirto = dirnum[dirto]
    if ndirto <= ndirfrom: ndirto += 4
    for _ in range(ndirfrom, ndirto):
        pos = rotate_pos_once(pos)
    return pos

def wrappos(area: int, pos : tuple[int,int], dir : str) -> tuple[int,str,tuple[int,int]]:
    global chunksize, conn
    row, col = pos
    is_outside = any([
        row < 0, row >= chunksize,
        col < 0, col >= chunksize
    ])
    if is_outside:
        if row < 0 : row = chunksize-1
        if row >= chunksize: row = 0
        if col < 0 : col = chunksize-1
        if col >= chunksize: col = 0
        newarea, newdirinv = conn[(area, dir)] #type: ignore
        newdir = dirinv[newdirinv]
        newpos = rotate_pos((row, col), dir, newdir)
        return newarea, newdir, newpos
    return area, dir, pos

def get_front(map : list[list[list[str]]], area : int, pos : tuple[int,int], dir : str):
    dirv = dirvec[dir]
    newarea, newdir, newpos = wrappos(area, addvec(pos, dirv), dir)
    frontchar = map[newarea][newpos[0]][newpos[1]]
    return newarea, newdir, newpos, frontchar

def move(map, area : int, pos : tuple[int,int], dir : str, n : int) -> tuple[int,str,tuple[int,int]]:
    print(area, pos, dir)
    if n == 0: return area, dir, pos
    frontarea, frontdir, frontpos, frontchar = get_front(map, area, pos, dir)
    # print(frontchar)
    if frontchar == '#': return area, dir, pos
    return move(map, frontarea, frontpos, frontdir, n-1)

dirvec = {
    'R': (0, 1),
    'D': (1, 0),
    'L': (0, -1),
    'U': (-1, 0)
}
dirchar = ['R', 'D', 'L', 'U']
dirnum = { 'R' : 0, 'D' : 1, 'L' : 2, 'U' : 3 }
dirinv = { 'R': 'L', 'L': 'R', 'U': 'D', 'D': 'U' }
addvec   = lambda a, b : (a[0]+b[0], a[1]+b[1])
scalevec = lambda s, v : (s*v[0], s*v[1])
turnR = lambda d : (d+1) % 4
turnL = lambda d : (d-1) % 4
# ------------------------------------------------

# chunksize = 4
# conn = conn_ex

chunksize = 50
conn = conn_inp
# with open('inpex.txt', 'r') as f:
with open('input.txt', 'r') as f:
    lines = f.read().splitlines()
(map, mapmeta), path = parsefile(lines)

map = [[]] + map
mapmeta = [(0,0)] + mapmeta
for m in map: print(m)
for m in mapmeta: print(m)
# mapcols = max([len(row) for row in map])
# map = [row + ' '*(mapcols-len(row)) for row in map]
#
print(path)
#
dir = 'R'
area = 1
pos = get_initial_pos(map[area])
print(pos)
#
for i,p in enumerate(path):
    if i%2 == 0:
        # number of steps
        n = int(p)
        area, dir, pos = move(map, area, pos, dir, n)
    else:
        # turn
        print('turning')
        if p == 'R': dir = dirchar[turnR(dirnum[dir])]
        elif p == 'L': dir = dirchar[turnL(dirnum[dir])]
        else: assert False, 'invalid turn'

row = pos[0]+1
col = pos[1]+1
row += mapmeta[area][0]
col += mapmeta[area][1]
passw = 1000*row + 4*col + dirnum[dir]
print(row, col, dir, passw)
