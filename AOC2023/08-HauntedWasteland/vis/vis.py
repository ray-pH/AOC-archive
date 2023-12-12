import pydot
import os

def generate_graph(network):
    graph = pydot.Dot("my_graph", graph_type="digraph")

    def add_node(name):
        if name[-1] == 'Z':
            graph.add_node(pydot.Node(name, label=name, style="filled", color="#ff9896"))
        elif name[-1] == 'A':
            graph.add_node(pydot.Node(name, label=name, style="filled", color="#aec7e8"))
        else:
            graph.add_node(pydot.Node(name, label=name))

    for name, left, right in network:
        add_node(name)
        add_node(left)
        add_node(right)
    for name, left, right in network:
        graph.add_edge(pydot.Edge(name, left, color="#1f77b4"))
        graph.add_edge(pydot.Edge(name, right, color="#d62728"))
    return(graph)

net = tuple[str, str, str]
def parse_network(networkstr):
    name, lrstr = networkstr.split(' = ')
    lrstr = lrstr[1:-1]
    left, right = lrstr.split(', ')
    return(name, left, right)

def f_get_isolated_network(network: list[net]) -> tuple[list[net], list[net]]:
    nd_count = 0
    name_done = {*network[0]}

    connected = []
    while nd_count != len(name_done):
        nd_count = len(name_done)
        not_connected = []
        for n in network:
            name, left, right = n
            if name in name_done or left in name_done or right in name_done:
                connected.append(n)
                name_done.add(name)
                name_done.add(left)
                name_done.add(right)
            else:
                not_connected.append(n)
        network = not_connected
    return connected, network

def get_isolated_networks(network: list[net]) -> list[list[net]]: 
    isolated_network = []
    while len(network) > 0:
        isolated, network = f_get_isolated_network(network)
        isolated_network.append(isolated)
    return isolated_network

# ----------------------------------------------

def generate_separate():
    print("generating separate graphs")
    # make sure ./sep/ exists, if not, create it
    if not os.path.exists("./sep/"): os.makedirs("./sep/")

    # with open('../inpex3.txt', 'r') as f:
    with open('../input.txt', 'r') as f:
        lines = f.read().splitlines()
    networkstr = lines[2:]
    network = list(map(parse_network, networkstr))
    print("done parsing network")
    isolated_networks = get_isolated_networks(network)
    print("done isolating networks")
    for i,nw in enumerate(isolated_networks):
        graph = generate_graph(nw)
        graph.write_raw(f"./sep/inp_{i}.dot") #type: ignore
        os.system(f"dot -Kneato -Tpng ./sep/inp_{i}.dot > ./sep/graph_{i}.png")
        os.system(f"dot -Kneato -Tsvg ./sep/inp_{i}.dot > ./sep/graph_{i}.svg")
        print(f"done graphing {i}")


def generate_single():
    print("generating single graph")
    # with open('../inpex3.txt', 'r') as f:
    with open('../input.txt', 'r') as f:
        lines = f.read().splitlines()
    networkstr = lines[2:]
    network = list(map(parse_network, networkstr))
    print("done parsing network")
    graph = generate_graph(network)

    graph.write_raw("input.dot") #type: ignore
    # run $ dot -Kneato -Tpng output.dot > out.png
    os.system("dot -Kneato -Tpng input.dot > graph.png")
    print("done graphing png")
    os.system("dot -Kneato -Tsvg input.dot > graph.svg")
    print("done graphing svg")

generate_single()
generate_separate()
