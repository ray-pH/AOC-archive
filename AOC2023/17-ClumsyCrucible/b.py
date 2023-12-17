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
dir_lr = {
    '>' : ['^', 'v'],
    '<' : ['v', '^'],
    '^' : ['<', '>'],
    'v' : ['>', '<']
}

t_v2 = tuple[int,int]
t_pos = t_v2
# head : pos, dir, count, dist
t_head = tuple[t_pos, str, int, int]
t_map = list[list[int]]

def add_vector(a : t_v2, b : t_v2) -> t_v2 : return (a[0]+b[0], a[1]+b[1])

def is_valid_head(head : t_head, map : t_map) -> bool:
    pos, dir, count, _ = head
    row_count = len(map)
    col_count = len(map[0])
    if pos[0] < 0: return False
    if pos[1] < 0: return False
    if pos[0] >= row_count: return False
    if pos[1] >= col_count: return False
    if dir == '>' and pos[1] + (4-count) >= col_count: return False
    if dir == '<' and pos[1] - (4-count) < 0: return False
    if dir == 'v' and pos[0] + (4-count) >= row_count: return False
    if dir == '^' and pos[0] - (4-count) < 0: return False
    return True

def update_distmap_nextheads(head : t_head, newcount : int, d : str, 
                             map : t_map, distmap : t_map, nextheads : set[t_head]):
    pos, _, _, dist = head
    newpos = add_vector(pos, dir_vector[d])
    if not is_valid_head((newpos, d, newcount, -1), map): return
    heat = map[newpos[0]][newpos[1]]
    newdist = dist + heat
    prev_dist = distmap[newpos[0]][newpos[1]]
    # if newdist >= prev_dist + 9*10: return
    # if newdist >= prev_dist + 9*5: return
    if newdist >= prev_dist + 9*3: return
    # newdist < prev_dist
    if newdist < prev_dist:
        distmap[newpos[0]][newpos[1]] = newdist
    newhead = newpos, d, newcount, newdist
    nextheads.add(newhead)

def next_heads(head : t_head, map : t_map, distmap : t_map) -> set[t_head]:
    _, dir, count, _ = head
    nextheads = set()
    if count < 10:
        # update_distmap_nextheads(head, newcount, dir, map, distmap, nextheads, count==4)
        update_distmap_nextheads(head, count+1, dir, map, distmap, nextheads)
    if count >= 4:
        for d in dir_lr[dir]:
            update_distmap_nextheads(head, 1, d, map, distmap, nextheads)
    return nextheads

def prune_heads(heads : list[t_head]) -> list[t_head]:
    if len(heads) <= 1: return heads
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


def traverse(init_heads : list[t_head], map : t_map, distmap : t_map) -> int:
    row_count = len(map)
    col_count = len(map[0])
    target = (row_count-1, col_count-1)
    heads = init_heads
    i = 0
    while True:
        heads = prune_heads(heads)
        # head : pos, dir, count, dist
        this_time_heads = [ head for head in heads if head[3] == i ]
        print(i, len(heads), len(this_time_heads))
        # print(heads)
        # for h in heads: print('   ', h)
        for head in this_time_heads:
            nextheads = next_heads(head, map, distmap)
            heads.remove(head)
            heads.extend(nextheads)
        if len(heads) == 0: raise RuntimeError("no heads")
        if any([head[0] == target for head in heads]): break
        i += 1
        # if i > 100: break
        # if i > 6: break
    # return(i+1)
    for h in heads:
        if h[0] == target:
            print(h)
            return h[3]
    return -1

# -------------- main -------------

# with open('inpex.txt', 'r') as f:
# with open('inpex2.txt', 'r') as f:
with open('input.txt', 'r') as f:
    lines = f.read().splitlines()
map = [[int(c) for c in line] for line in lines]
total = sum([sum(line) for line in map])
distmap = [[total for _ in line] for line in lines]

init_head : t_head = (0,0), '>', 0, 0
heads : list[t_head]= [init_head]
print(map)
t = traverse(heads, map, distmap)
print(t)
