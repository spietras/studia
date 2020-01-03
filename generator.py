import argparse
import random
import itertools
import sys


def generate(bipartite, n, first_n, density):
    if density is None:
        density = random.random()

    if bipartite:
        if first_n is None:
            first_n = random.randint(0, n)

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


if __name__ == '__main__':
    def parse_args_solvable(parser, args):

        if not args.n >= 1:
            parser.error("Minimum card number is 1")
        if args.d is not None and not (0.0 <= args.d <= 1.0):
            parser.error("Restrictions density has to be between 0.0 and 1.0")
        if args.f is not None and not (0 <= args.f <= args.n):
            parser.error("Numer of substances in the first magazine has to be between 0 and number of all substances")

        return args

    def parse_args_unsolvable(parser, args):

        if not args.n >= 1:
            parser.error("Minimum card number is 1")
        if args.d is not None and not (0.0 <= args.d <= 1.0):
            parser.error("Restrictions density has to be between 0.0 and 1.0")

        return args


    parser = argparse.ArgumentParser()

    subparsers = parser.add_subparsers()
    solvable_parser = subparsers.add_parser('solvable')
    unsolvable_parser = subparsers.add_parser('unsolvable')

    solvable_parser.set_defaults(mode='solvable')
    unsolvable_parser.set_defaults(mode='unsolvable')

    for p in [solvable_parser, unsolvable_parser]:
        p.add_argument("n", type=int, help="number of substances")
        p.add_argument("-d", type=float, help="density of restrictions (0.0 - no restrictions, 1.0 - max restrictions)")

    solvable_parser.add_argument("-f", type=int, help="number of substances in the first magazine")

    args = parser.parse_args()

    if len(sys.argv) == 1:
        parser.print_usage()
        parser.exit()

    if args.mode == 'solvable':
        parse_args_solvable(parser, args)

        generated = generate(True, args.n, args.f, args.d)
    elif args.mode == 'unsolvable':
        parse_args_unsolvable(parser, args)

        generated = generate(False, args.n, None, args.d)
    else:
        parser.print_usage()
        parser.exit()

    print('\n'.join('{} {}'.format(*restriction) for restriction in generated))
