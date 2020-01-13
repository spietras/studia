"""
Module with algorithm solving substance displacement problem

Author: Sebastian Pietras
Project: Chemikalia
"""

from collections import deque

from graph import Graph


def T(n, density):
    """
    Get theoretical complexity

    Parameters:
        n - input size (substances number)
        density - density of restrictions
    """
    possible_edges = int(n / 2) * (n - int(n / 2))
    e = int(density * possible_edges)

    return n + e  # n + n^2 / 4 for all possible edges


def solve(v, edges):  # O(v + e) average
    """
    Solve substance displacement problem

    Parameters:
        v - number of substances
        edges - restrictions
    """
    graph = Graph(v, edges)  # O(v + e) average
    return get_graph_parts(graph)  # O(v + e) average


def get_graph_parts(graph):  # O(v + e) average
    """Get two-coloring of whole graph"""
    nodes = graph.get_nodes()  # O(v)
    set1 = set()  # O(1)
    set2 = set()  # O(1)

    while nodes:  # O(v + e) average

        # complexity of this statement depends on the size of subgraph containing passed node
        # but here we iterate through every node in the original graph
        # so after summing up the complexity of this statement for the whole loop is O(v + e) average
        bad_edge, subset1, subset2 = get_subtree_parts(graph, nodes.pop())

        # add found subsets to whole set
        # complexity depends of subset size
        # adding n elements to set of size m is O(m+n)
        # after summing up subsets should contain every node
        # so complexity for the whole loop is O(v)
        set1.update(subset1)
        set2.update(subset2)

        if bad_edge:
            return bad_edge, set1, set2

        # remove visited nodes from consideration
        # complexity depends of subset sizes
        # subtracting n elements is O(n)
        # after summing up subsets should contain every node
        # so complexity for the whole loop is O(v)
        nodes.difference_update(subset1, subset2)

    return None, set1, set2


def get_subtree_parts(graph, start_node):
    """
    Get two-coloring of subtree

    Parameters:
        graph - graph object
        start_node - node the subtree contains
    """
    color_a = {start_node}  # O(1)
    color_b = set()  # O(1)

    queue = deque([start_node])  # O(1)

    # BFS - go through the graph and add neighbours to queue

    while queue:  # for each node in subgraph - after summing up this loop will iterate through every node
        node = queue.popleft()  # O(1)

        this_color, other_color = (color_a, color_b) if node in color_a else (color_b, color_a)  # O(1) average

        for neighbour in graph.get_neighbours(
                node):  # for each neighbour of node - after summing up this loop will iterate through every edge

            # if this is true, there is a neighbour in the same color as this node so two-coloring is not possible
            if neighbour in this_color:  # O(1) average
                return (node, neighbour), color_a, color_b

            # if this is true, the neighbour was not visited yet, so color him and add to the queue
            if neighbour not in other_color:  # O(1) average
                other_color.add(neighbour)  # O(1) average
                queue.append(neighbour)  # O(1)

    return None, color_a, color_b
