class Graph:
    def __init__(self, v, edges):  # O(v + e) average
        self._v = v

        self._adj_list = {i: set() for i in range(1, v + 1)}  # loop v times: O(v)

        for begin, end in edges:  # loop e times: O(e) average
            self._adj_list[begin].add(end)  # dict access and set add: O(1) average
            self._adj_list[end].add(begin)  # same as above

    def get_neighbours(self, node):
        return self._adj_list[node]  # O(1) average

    def get_nodes(self):
        return set(self._adj_list.keys())  # list to set: O(v)
