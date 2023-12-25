import pydot
import os

def generate_newtork(lines : list[str]) :
    network = []
    for line in lines:
        name,rstr = line.split(': ')
        targets = rstr.split(' ')
        network.append((name, targets))
    return network

bgcolor  = "#0f0f23"
grcolor1 = "#009900"
grcolor2 = "#55aa55"
ylcolor1 = "#e7e75f"
ylcolor2 = "#e7e75f"
whcolor  = "#ffffff"
special = [
    ["hfx","pzl"],
    ["bvb","cmg"],
    ["nvd","jqt"],
]

def generate_graph(network):
    graph = pydot.Dot("my_graph", graph_type="graph", bgcolor=bgcolor)

    def add_node(name):
        graph.add_node(pydot.Node(name, label=name, style="filled", color=ylcolor2, fontcolor=bgcolor))

    # addednode = set()
    for name, targets in network:
        add_node(name)
        for t in targets:
            c = whcolor
            if [name,t] in special or [t,name] in special:
                c = grcolor1
            add_node(t)
            graph.add_edge(pydot.Edge(name, t, color=c))
    return graph


with open('./input.txt', 'r') as f:
# with open('./inpex.txt', 'r') as f:
    lines = f.read().splitlines()
network = generate_newtork(lines)
graph = generate_graph(network)
graph.write_raw("probe.dot") #type: ignore

# os.system(f"dot -Kneato -Tsvg ./sep/inp_{i}.dot > ./sep/graph_{i}.svg")
