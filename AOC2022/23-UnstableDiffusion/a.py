def parse_map(map):
    elfs = set()
    for j in range(len(map)):
        for i in range(len(map[j])):
            if map[j][i] == '#':
                elfs.add((i, j))
    return elfs

vecdir = {
    'NW': (-1, -1), 'N' : (0, -1), 
    'NE': (1, -1), 'E' : (1, 0), 
    'SE': (1, 1), 'S' : (0, 1),
    'SW': (-1, 1), 'W' : (-1, 0),
}
diradjacent = {
    'N' : ['N', 'NE', 'NW'],
    'S' : ['S', 'SE', 'SW'],
    'W' : ['W', 'NW', 'SW'],
    'E' : ['E', 'NE', 'SE'],
}
addvec = lambda a, b: (a[0] + b[0], a[1] + b[1])

def cycle(list):
    return list[1:] + [list[0]]

# emptydir = { N : bool, E : bool, S : bool, W : bool }
# true if that direction is empty
def get_emptydir(pos, elfs):
    emptydir = {}
    for dir, adjdir in diradjacent.items():
        emptydir[dir] = not any([addvec(pos, vecdir[d]) in elfs for d in adjdir])
    return emptydir

def get_movement(elfs, order):
    movement = set()
    for e in elfs:
        emptydir = get_emptydir(e, elfs)
        allempty = all(emptydir.values())
        if allempty: continue
        for dir in order:
            if emptydir[dir]:
                target = addvec(e, vecdir[dir])
                if target not in elfs: movement.add((e, target))
                break
    return movement

# return only unique elements from a list, remove if count is more than 1
def only_unique(arr):
    return {x for x in arr if arr.count(x) == 1}

def move_single(elfs, movement):
    for m in movement:
        elfs.remove(m[0])
        elfs.add(m[1])
    return elfs

def move(elfs, n):
    order = ['N', 'S', 'W', 'E']
    for _ in range(n):
        movement = get_movement(elfs, order)
        targets = [m[1] for m in movement]
        unique_targets = only_unique(targets)
        valid_movement = {m for m in movement if m[1] in unique_targets}
        elfs = move_single(elfs, valid_movement)
        order = cycle(order)
    return elfs

def bbox(elfs):
    minx = min([e[0] for e in elfs])
    maxx = max([e[0] for e in elfs])
    miny = min([e[1] for e in elfs])
    maxy = max([e[1] for e in elfs])
    return (minx, maxx, miny, maxy)

def calc_empty(elfs):
    minx, maxx, miny, maxy = bbox(elfs)
    return (maxx - minx + 1) * (maxy - miny + 1) - len(elfs)

#------------------------ visualization
def fprint(elfs):
    minx, maxx, miny, maxy = bbox(elfs)
    for j in range(miny, maxy + 1):
        for i in range(minx, maxx + 1):
            if (i, j) in elfs: 
                print('#', end='')
            else: print('.', end='')
        print()

# ------------------------------------

# for l in lines: print(l)
# with open('inpex.txt', 'r') as f:
# with open('inpex2.txt', 'r') as f:
with open('input.txt', 'r') as f:
    lines = f.read().splitlines()


elfs = parse_map(lines)
fprint(elfs)
print()
elfs = move(elfs, 10)
fprint(elfs)
print(calc_empty(elfs))


