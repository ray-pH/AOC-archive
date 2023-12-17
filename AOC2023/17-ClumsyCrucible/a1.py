dirs = ['>', '<', '^', 'v']
dir_vector = {
    '>' : (0,1),
    '<' : (0,-1),
    '^' : (-1,0),
    'v' : (1,0)
}
dir_reverse = {
    '>' : '<',
    '<' : '>',
    '^' : 'v',
    'v' : '^'
}

t_v2 = tuple[int,int]
t_pos = t_v2
# head : pos, dir, count, dist
t_head = tuple[t_pos, str, int, int]
t_map = list[list[int]]

def add_vector(a : t_v2, b : t_v2) -> t_v2 : return (a[0]+b[0], a[1]+b[1])

def is_valid_pos(pos : t_pos, map : t_map) -> bool:
    mapsize = len(map)
    if pos[0] < 0 or pos[0] >= mapsize: return False
    if pos[1] < 0 or pos[1] >= mapsize: return False
    return True

def next_heads(head : t_head, map : t_map, distmap : t_map) -> set[t_head]:
    pos, dir, count, dist = head
    rdir = dir_reverse[dir]
    nextheads = set()
    for d in dirs:
        if d == rdir: continue
        newpos = add_vector(pos, dir_vector[d])
        newcount = count + 1 if d == dir else 1
        if newcount > 3: continue
        if not is_valid_pos(newpos, map): continue
        heat = map[newpos[0]][newpos[1]]
        newdist = dist + heat
        prev_dist = distmap[newpos[0]][newpos[1]]
        if newdist >= prev_dist + 18: continue
        # newdist < prev_dist
        if newdist < prev_dist:
            distmap[newpos[0]][newpos[1]] = newdist
        newhead = newpos, d, newcount, newdist
        nextheads.add(newhead)
    return nextheads

def prune_heads(heads : list[t_head]) -> list[t_head]:
    heads = sorted(heads)
    pruned = [heads[0]]
    for i in range(1,len(heads)):
        ppos, pdir, pcount, pdist = pruned[-1]
        pos, dir, count, dist = heads[i]
        if (pos == ppos) and (dir == pdir) and (count == pcount):
            if dist < pdist: 
                pruned[-1] = heads[i]
                assert False, "i assume this is unreachable"
        else:
            pruned.append(heads[i])
    return pruned

def add_new_heads(nextheads : set[t_head], heads : dict[int, set[t_head]]):
    for h in nextheads:
        t = h[3]
        if t not in heads: heads[t] = set()
        heads[t].add(h)

def traverse(init_heads : dict[int, set[t_head]], map : t_map, distmap : t_map) -> int:
    mapsize = len(map)
    target = (mapsize-1, mapsize-1)
    heads = init_heads
    i = 0
    while True:
        # heads = prune_heads(heads)
        # head : pos, dir, count, dist
        # this_time_heads = [ head for head in heads if head[3] == i ]
        while i not in heads: i += 1
        this_time_heads = heads[i]
        print(i, len(heads), len(this_time_heads))
        for head in this_time_heads:
            nextheads = next_heads(head, map, distmap)
            add_new_heads(nextheads, heads)
            # heads.extend(nextheads)
        if any([head[0] == target for head in heads[i]]): break
        i += 1
        # if i > 100: break
        # if i > 10: break
    return(i+1)

# -------------- main -------------

with open('inpex.txt', 'r') as f:
# with open('input.txt', 'r') as f:
    lines = f.read().splitlines()
map = [[int(c) for c in line] for line in lines]
total = sum([sum(line) for line in map])
distmap = [[total for _ in line] for line in lines]

init_t = map[0][0]
init_head : t_head = (0,0), '>', 0, map[0][0]
heads : dict[int, set[t_head]] = {}
heads[init_t] = {init_head}
print(map)
t = traverse(heads, map, distmap)
print(t)
