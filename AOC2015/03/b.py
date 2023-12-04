
with open('input.txt', 'r') as f:
    lines = f.readlines()
    route = lines[0][:-1]
route_a = route[::2]
route_b = route[1::2]

direction = {
    '^': (0,1),
    'v': (0,-1),
    '>': (1,0),
    '<': (-1,0)
}

pos_a = (0,0)
pos_b = (0,0)
visited = {(0,0)}

for dir in route_a:
    pos_a = (pos_a[0] + direction[dir][0], pos_a[1] + direction[dir][1])
    visited.add(pos_a)
for dir in route_b:
    pos_b = (pos_b[0] + direction[dir][0], pos_b[1] + direction[dir][1])
    visited.add(pos_b)
print(len(visited))
