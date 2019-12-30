class Graph:
    def __init__(self, v, edges):
        self._v = v

        self._adj_list = {i: set() for i in range(1, v + 1)}

        for begin, end in edges:
            self._adj_list[begin].add(end)
            self._adj_list[end].add(begin)

    def get_neighbours(self, node):
        return self._adj_list[node]

    def get_nodes(self):
        return set(self._adj_list.keys())
