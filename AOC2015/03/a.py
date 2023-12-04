
with open('input.txt', 'r') as f:
    lines = f.readlines()
    route = lines[0][:-1]

direction = {
    '^': (0,1),
    'v': (0,-1),
    '>': (1,0),
    '<': (-1,0)
}

pos = (0,0)
visited = {pos}

for dir in route:
    pos = (pos[0] + direction[dir][0], pos[1] + direction[dir][1])
    visited.add(pos)
print(len(visited))
