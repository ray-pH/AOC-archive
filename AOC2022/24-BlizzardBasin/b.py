def get_symbol_positions(sym, lines):
    pos = set()
    for i, line in enumerate(lines):
        for j, c in enumerate(line):
            if c == sym: pos.add((i, j))
    return pos

def parse_map(lines):
    up = get_symbol_positions('^', lines)
    down = get_symbol_positions('v', lines)
    left = get_symbol_positions('<', lines)
    right = get_symbol_positions('>', lines)
    return up, down, left, right

def move_up(height, pos):
    newrow, newcol = pos[0] - 1, pos[1]
    if newrow == 0: newrow = height - 2
    return (newrow, newcol)
def move_down(height, pos):
    newrow, newcol = pos[0] + 1, pos[1]
    if newrow == height - 1: newrow = 1
    return (newrow, newcol)
def move_left(width, pos):
    newrow, newcol = pos[0], pos[1] - 1
    if newcol == 0: newcol = width - 2
    return (newrow, newcol)
def move_right(width, pos):
    newrow, newcol = pos[0], pos[1] + 1
    if newcol == width - 1: newcol = 1
    return (newrow, newcol)

def move_blizzard(mapsize, up, down, left, right):
    height, width = mapsize
    up = {move_up(height, pos) for pos in up}
    down = {move_down(height, pos) for pos in down}
    left = {move_left(width, pos) for pos in left}
    right = {move_right(width, pos) for pos in right}
    return up, down, left, right

def has_blizzard(pos, blizzards):
    return any(pos in b for b in blizzards)

def genmove(pos):
    pup = pos[0]-1, pos[1]
    pdown = pos[0]+1, pos[1]
    pleft = pos[0], pos[1]-1
    pright = pos[0], pos[1]+1
    pwait = pos[0], pos[1]
    return [pup, pdown, pleft, pright, pwait]
def genpossiblemoves(pos, mapsize, blizzards):
    moves = genmove(pos)
    has_blizzard = lambda p : any(p in b for b in blizzards)
    is_outside   = lambda p : p[0] < 1 or p[0] >= mapsize[0]-1 or p[1] < 1 or p[1] >= mapsize[1]-1
    is_inout     = lambda p : p == (0,1) or p == (mapsize[0]-1, mapsize[1]-2)
    is_valid     = lambda p : is_inout(p) or (not has_blizzard(p) and not is_outside(p))
    return [p for p in moves if is_valid(p)]
def iter_single(pos, up, down, left, right, mapsize):
    blizzards = [up, down, left, right]
    possiblemoveslist = [genpossiblemoves(p, mapsize, blizzards) for p in pos]
    pos = set(sum(possiblemoveslist, []))
    up, down, left, right = move_blizzard(mapsize, up, down, left, right)
    return pos, up, down, left, right

def iter(i, pos, target, up, down, left, right, mapsize):
    while target not in pos:
        pos, up, down, left, right = iter_single(pos, up, down, left, right, mapsize)
        print(i, target, len(pos))
        i = i + 1
    return i-1, pos, up, down, left, right

# --------------------------

# with open('inpex.txt', 'r') as f:
# with open('inpex2.txt', 'r') as f:
with open('input.txt', 'r') as f:
    lines = f.read().splitlines()

mapsize = len(lines), len(lines[0])

up, down, left, right = parse_map(lines)
pos = {(0, 1)}
target = mapsize[0]-1, mapsize[1]-2
i, pos, up, down, left, right =\
    iter(0, pos, target, up, down, left, right, mapsize)

pos = {target}
target = (0, 1)
i, pos, up, down, left, right =\
    iter(i+1, pos, target, up, down, left, right, mapsize)

pos = {(0, 1)}
target = mapsize[0]-1, mapsize[1]-2
i, pos, up, down, left, right =\
    iter(i+1, pos, target, up, down, left, right, mapsize)
