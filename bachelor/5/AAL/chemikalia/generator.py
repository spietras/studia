"""
Module with generator of substance displacement problems

Author: Sebastian Pietras
Project: Chemikalia
"""

import argparse
import itertools
import random


def generate(bipartite, n, first_n, density):
    """
    Generate edges of graph

    Parameters:
        bipartite - should graph be bipartite (True/False)
        n - number of vertices
        first_n - number of vertices in one part (ignored if bipartite == False)
        density - density of edges (0.0 to 1.0)
    """
    if density is None:
        density = random.random()

    if bipartite:
        if first_n is None:
            first_n = random.randint(0, n)

        return generate_bipartite(n, first_n, density)

    return generate_non_bipartite(n, density)


def generate_bipartite(n, first_n, density):
    """
    Generate edges of bipartite graph

    Parameters:
        n - number of vertices
        first_n - number of vertices in one part
        density - density of edges (0.0 to 1.0)
    """
    vertices = [i + 1 for i in range(n)]

    x = random.sample(vertices, n)  # random shuffling of vertices

    first = x[:first_n]  # first part of vertices
    second = x[first_n:]  # second part of vertices

    possible_edges = list(itertools.product(first, second))  # edges as product between parts

    picked_edges = random.sample(possible_edges, int(density * len(possible_edges)))

    return picked_edges


def generate_non_bipartite(n, density):
    """
    Generate edges of non-bipartite graph

    Parameters:
        n - number of vertices
        density - density of edges (0.0 to 1.0)
    """
    vertices = [i + 1 for i in range(n)]

    # pick 3 vertices and connect them so graph can't be bipartite
    bad_vertices = random.sample(vertices, 3)
    bad_vertices_edges = {tuple(sorted(edge)) for edge in itertools.combinations(bad_vertices, 2)}

    # make all other possible edges excluding picked above (to avoid duplicates)
    all_edges = {tuple(sorted(edge)) for edge in itertools.combinations(vertices, 2)}
    possible_edges = all_edges - bad_vertices_edges

    picked_edges = random.sample(possible_edges, int(density * len(possible_edges))) + list(bad_vertices_edges)

    return picked_edges


def parse_args_solvable(parser, args):
    if not args.n >= 1:
        parser.error("Minimum card number is 1")
    if args.density is not None and not (0.0 <= args.density <= 1.0):
        parser.error("Restrictions density has to be between 0.0 and 1.0")
    if args.first is not None and not (0 <= args.first <= args.n):
        parser.error("Numer of substances in the first magazine has to be between 0 and number of all substances")

    return args


def parse_args_unsolvable(parser, args):
    if not args.n >= 1:
        parser.error("Minimum card number is 1")
    if args.density is not None and not (0.0 <= args.density <= 1.0):
        parser.error("Restrictions density has to be between 0.0 and 1.0")

    return args


def create_parser():
    parser = argparse.ArgumentParser(description="Generate test data for substance displacement problem")

    subparsers = parser.add_subparsers(dest="type")
    solvable_parser = subparsers.add_parser('solvable', help="generate solvable problem",
                                            description="Generate test data for solvable substance displacement problem")
    unsolvable_parser = subparsers.add_parser('unsolvable', help="generate unsolvable problem",
                                              description="Generate test data for unsolvable substance displacement problem")

    for p in [solvable_parser, unsolvable_parser]:
        p.add_argument("n", type=int, help="number of substances")
        p.add_argument("-d", "--density", type=float,
                       help="density of restrictions (0.0 - no restrictions, 1.0 - max restrictions)")

    solvable_parser.add_argument("-f", "--first", type=int, help="number of substances in the first magazine")
    return parser, {'solvable': solvable_parser, 'unsolvable': unsolvable_parser}


if __name__ == '__main__':
    parser, subparsers = create_parser()
    args = parser.parse_args()

    if args.type == 'solvable':
        args = parse_args_solvable(subparsers['solvable'], args)

        generated = generate(True, args.n, args.first, args.density)
    elif args.type == 'unsolvable':
        args = parse_args_unsolvable(subparsers['unsolvable'], args)

        generated = generate(False, args.n, None, args.density)
    else:
        parser.print_usage()
        parser.exit()

    print('\n'.join('{} {}'.format(*restriction) for restriction in generated))
