from collections import deque

from graph import Graph


def solve(v, edges):
    graph = Graph(v, edges)
    return get_graph_parts(graph)


def get_graph_parts(graph):
    nodes = graph.get_nodes()
    set1 = set()
    set2 = set()

    while nodes:
        bad_edge, subset1, subset2 = get_subtree_parts(graph, nodes.pop())

        set1.update(subset1)
        set2.update(subset2)

        if bad_edge:
            return bad_edge, set1, set2

        nodes.difference_update(subset1, subset2)

    return None, set1, set2


def get_subtree_parts(graph, start_node):
    color_a = {start_node}
    color_b = set()

    queue = deque([start_node])

    while queue:
        node = queue.popleft()

        this_color, other_color = (color_a, color_b) if node in color_a else (color_b, color_a)

        for neighbour in graph.get_neighbours(node):
            if neighbour in this_color:
                return (node, neighbour), color_a, color_b

            if neighbour not in other_color:
                other_color.add(neighbour)
                queue.append(neighbour)

    return None, color_a, color_b
