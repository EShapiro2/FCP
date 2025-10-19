1. Given a tree in the Newick format -> generate a dictionary of all the tree's quartets 
	real_quartet_dictionary = {frozenset([c1,c2,c3,c4]): frozenset([frozenset([c1,c2]), frozenset([c3,c4])])}
	this dictionary allows us to query for every quartet (c1,c2,c3,c4) what is the topology ((c1,c2),(c3,c4))

2. implement the function check_quartet:
	check_quartet(c1,c2,c3,c4, real_quartet_dictionary):
		quartet_topology = decide_on_quartet(c1,c2,c3,c4)
		if quartet_topology == real_quartet_dictionary[frozenset([c1,c2,c3,c4])]:  # compare computed quartet topology with real quartet in real_quartet_dictionary
		
		
		    ...
			
			import pickle
import matplotlib.pyplot as plt
import networkx as nx

from calibrations.triplets_baseline import get_tree_from_newick, create_tree_triplets_dictionary
from solid.utils import subdict


def show_graph(G):
    print 'drawing'
    layout = nx.spring_layout(G)
    nx.draw(G, pos=layout)
    nx.draw_networkx_labels(G, pos=layout)
    plt.show()


def get_cells_and_root(calling):
    # Verify the presence of a root cell in the input data.
    assert 'root' in calling
    cells = []
    for cell in calling:
        if cell == 'root':
            root = (cell, calling[cell])
            continue
        cells.append((cell, calling[cell]))
    return root, cells


def parse_and_dump_real_triplets(newick_file, triplets_file):
    # read tree as newick file and dump a triplets dictionary to file
    real_tree = get_tree_from_newick(newick_file)
    tris2 = create_tree_triplets_dictionary(real_tree)
    pickled_string = pickle.dumps(tris2)
    with open(triplets_file, 'wb') as f:
        f.write(pickled_string)


def load_real_triplets(path):
    # Load real triplets from ex-vivo tree
    with open(path, 'rb') as f:
            pickled_string = f.read()
    tris = pickle.loads(pickled_string)
    return tris


def get_shared(cells, loci=set()):
    if not loci:
        loci = set.intersection(*[set(cell[1]) for cell in cells])
    new_cells = []
    for cell in cells:
        new_cells.append((cell[0], subdict(cell[1], loci)))
    return new_cells, loci


def get_classifying_loci(cells):
    assert len(cells) > 1
    loci = set.intersection(*[set(cell[1]) for cell in cells])
    clc = []
    for loc in loci:
        v = cells[0][1][loc]
        if all([cell[1][loc] == v for cell in cells]):
            continue
        clc.append(loc)
    return clc


# def get_average_root(cells):
#     loci = set.union(*[set(cell[1]) for cell in cells])
#     root_loci = {}
#     for loc in loci:
#         loc_values = []
#         for cell in cells:
#             if cell[1][loc]:
#                 loc_values.append(cell[1][loc])
#         root_loci[loc] =