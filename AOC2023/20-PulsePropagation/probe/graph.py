import pydot
import os

def generate_newtork(lines : list[str]) :
    network = []
    for line in lines:
        lstr, rstr = line.split(' -> ')
        typ = lstr[0]
        name = lstr.lstrip('%&')
        targets = rstr.split(', ')
        network.append((typ, name, targets))
    return network

bgcolor  = "#0f0f23"
grcolor1 = "#009900"
grcolor2 = "#55aa55"
ylcolor1 = "#e7e75f"
ylcolor2 = "#e7e75f"
whcolor  = "#ffffff"
def generate_graph(network):
    graph = pydot.Dot("my_graph", graph_type="digraph", bgcolor=bgcolor)

    def add_node(typ, name):
        if typ == '&':
            graph.add_node(pydot.Node(name, label=name, style="filled", color=ylcolor2, fontcolor=bgcolor))
        elif typ == '%':
            graph.add_node(pydot.Node(name, label=name, style="filled", color=grcolor2, fontcolor=whcolor))
        else:
            graph.add_node(pydot.Node(name, label=name, fontcolor=whcolor, color=whcolor))

    addednode = set()
    for typ, name, targets in network:
        if name == 'broadcaster' : continue
        add_node(typ, name)
        addednode.add(name)
    for typ, name, targets in network:
        if name == 'broadcaster' : 
            for i,t in enumerate(targets):
                if t not in addednode: add_node('b', t)
                graph.add_edge(pydot.Edge(name+str(i), t, color=grcolor1))
            continue
        for t in targets:
            if t not in addednode: add_node('b', t)
            graph.add_edge(pydot.Edge(name, t, color=grcolor1))
    return graph


with open('../input.txt', 'r') as f:
    lines = f.read().splitlines()
network = generate_newtork(lines)
graph = generate_graph(network)
graph.write_raw("probe.dot") #type: ignore

# os.system(f"dot -Kneato -Tsvg ./sep/inp_{i}.dot > ./sep/graph_{i}.svg")
