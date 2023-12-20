import networkx as nx
import matplotlib.pyplot as plt
from pyvis.network import Network
from itertools import chain

with open('aoc2023/inputs/day20/input.txt') as f:
    c = f.read().splitlines()

def nodeType(n):
    return n[0]

def trimLine(l):
    if l[0] in '%&':
        return l[1:]
    return l

def lineToEdges(l):
    source, ts = l.split(' -> ')
    targets = ts.split(', ')
    return [(source, t) for t in targets]

def withType(n, node_types):
    return f'{n} ({node_types[n]})'

nodes = list(map(lambda l: l.split(' -> ')[0], c))
node_types = {trimLine(n): nodeType(n) for n in nodes}
node_types['rx'] = 'o'

c = list(map(trimLine, c))
edges = list(chain.from_iterable(list(map(lineToEdges, c))))
typedEdges = list(map(lambda x: (withType(x[0], node_types), withType(x[1], node_types)), edges))

G = nx.DiGraph()
G.add_edges_from(typedEdges)
pos = nx.spring_layout(G)
nx.draw_networkx(G, pos, arrows=True)
#nx.draw_networkx_nodes(G, pos)
#nx.draw_networkx_labels(G, pos)
#nx.draw_networkx_edges(G, pos, arrows=True)
net = Network(
    directed = True,
    #select_menu = True, # Show part 1 in the plot (optional)
    #filter_menu = True, # Show part 2 in the plot (optional)
)
#net.show_buttons(False) # Show part 3 in the plot (optional)
net.from_nx(G) # Create directly from nx graph
net.show('test.html', notebook=False)
#plt.show()

#for e in edges:
#    print(e[0], e[1])

