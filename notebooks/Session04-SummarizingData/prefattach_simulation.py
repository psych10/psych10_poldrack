"""
a simulation of preferential
attachment in a network
"""

import numpy
import networkx as nx

nNodes=5000

G=nx.barabasi_albert_graph(nNodes,2)

degreeDist=numpy.array(list(G.degree().values()))
numpy.savetxt('bagraph_degree.txt',degreeDist)

randGraph=nx.fast_gnp_random_graph(nNodes,
    numpy.mean(degreeDist)/len(degreeDist))
degreeDistRandom=numpy.array(list(randGraph.degree().values()))
numpy.savetxt('randgraph_degree.txt',degreeDistRandom)
