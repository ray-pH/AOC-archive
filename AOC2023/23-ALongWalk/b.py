import networkx as nx
import matplotlib.pyplot as plt
from networkx.classes.function import path_weight

def parse_data(pdata):
    result = []
    for i in range(1,len(pdata),2):
        row = pdata[i]
        for j in range(0,len(row),3):
            src = row[j]
            tgt = row[j+1]
            weight = row[j+2]
            result.append((src,tgt,weight))
    return result

pos = tuple[int,int]
def generate_graph(data : list[tuple[pos,pos,int]]):
    added_node = set()
    added_edge = set()
    G = nx.Graph()
    for src,tgt,weight in data:
        if src not in added_node:
            G.add_node(src)
            added_node.add(src)
        if tgt not in added_node:
            G.add_node(tgt)
            added_node.add(tgt)
        if (tgt,src) not in added_edge:
            G.add_edge(src,tgt,weight=weight)
            added_edge.add((src,tgt))
    return G


with open('./graph.txt', 'r') as f:
    lines = f.read().split('\n')
    line = lines[0]
pdata = eval(line)
data = parse_data(pdata)
print(data)
graph = generate_graph(data)
nx.draw(graph)

start = (0,1)
end = (140,139)
maxx = 0
maxpath = None
maxcount = 1
for i, path in enumerate(nx.all_simple_paths(graph, source=start, target=end)):
    dist = path_weight(graph,path, weight='weight')
    if dist == maxx:
        maxcount += 1
    if dist > maxx:
        maxx = dist
        maxpath = path
        maxcount = 1
    print(i, maxx, maxcount)
print(maxpath)
