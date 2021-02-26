"""
Module containing graph definition

Author: Sebastian Pietras
Project: Chemikalia
"""


class Graph:
    """
    Graph class based on adjacency list

    Attributes:
        v - number of vertices
        adj_list - dictionary of vertices and their neighbours
    """
    def __init__(self, v, edges):  # O(v + e) average
        self._v = v

        self._adj_list = {i: set() for i in range(1, v + 1)}  # loop v times: O(v)

        for begin, end in edges:  # loop e times: O(e) average
            self._adj_list[begin].add(end)  # dict access and set add: O(1) average
            self._adj_list[end].add(begin)  # same as above

    def get_neighbours(self, node):
        """Get neighbours of given node"""
        return self._adj_list[node]  # O(1) average

    def get_nodes(self):
        """Get set of nodes in the graph"""
        return set(self._adj_list.keys())  # list to set: O(v)
