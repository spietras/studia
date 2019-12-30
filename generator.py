import random
import itertools


def generate(bipartite, n, first_n, density):
    if bipartite:
        return generate_bipartite(n, first_n, density)

    return generate_non_bipartite(n, density)


def generate_bipartite(n, first_n, density):
    vertices = [i + 1 for i in range(n)]

    x = random.sample(vertices, n)

    first = x[:first_n]
    second = x[first_n:]

    possible_edges = list(itertools.product(first, second))

    picked_edges = random.sample(possible_edges, int(density * len(possible_edges)))

    return picked_edges


def generate_non_bipartite(n, density):
    vertices = [i + 1 for i in range(n)]

    bad_vertices = random.sample(vertices, 3)

    possible_edges = list(itertools.combinations(vertices, 2))

    picked_edges = random.sample(possible_edges, int(density * len(possible_edges))) + list(
        itertools.combinations(bad_vertices, 2))

    return picked_edges
