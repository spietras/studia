import argparse
import sys
import generator
import algorithm
import measuring


def solve_given(n, restrictions):
    return algorithm.solve(n, restrictions)


def generate_and_solve(solvable, n, first_n, density):
    return algorithm.solve(n, generator.generate(solvable, n, first_n, density))


def measure(start_n, iterations, step, density, instances, repeats_per_instance=10):
    points = [n for n in range(start_n, start_n + iterations * step, step)]
    times = []
    for n in points:
        def algorithm_fun(edges, v=n): algorithm.solve(v, edges)
        def generator_fun(v=n, d=density): return generator.generate(True, v, int(v / 2), d)
        times.append(measuring.measure_generated(algorithm_fun, generator_fun, instances, repeats_per_instance))

    return list(zip(points, times))


def parse_args_m1(parser, args):
    if not args.n >= 1:
        parser.error("Minimum substance number is 1")

    restrictions = []
    with open(args.i, 'r') if args.i is not None else sys.stdin as f:
        for line in f:
            substances_string_list = line.split()

            try:
                substances_string_int = [int(x) for x in substances_string_list]
                if len(substances_string_int) != 2:
                    raise ValueError("Restriction {} is not a pair".format(substances_string_int))
                for x in substances_string_int:
                    if not (1 <= x <= args.n):
                        raise ValueError("'{}' is outside of range".format(x))
            except ValueError as ex:
                parser.error("Restriction {} should be a pair of integers from 1 to n splitted by space. {}".format(
                    substances_string_list, ex))

            if substances_string_list:
                restrictions.append(tuple(substances_string_int))

    args.i = restrictions

    return args


def parse_args_m3(parser, args):
    if not args.n >= 1:
        parser.error("Minimum card number is 1")
    if not args.iterations >= 1:
        parser.error("Minimum iterations number is 1")
    if not args.step >= 1:
        parser.error("Increment has to be a positive integer")
    if not (0.0 <= args.density <= 1.0):
        parser.error("Restrictions density has to be between 0.0 and 1.0")
    if args.instances is not None and not args.instances >= 1:
        parser.error("Minimum instances number is 1")

    return args


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Substance displacement problem")

    generator_parser, generator_subparsers = generator.create_parser()

    subparsers = parser.add_subparsers(dest="mode")
    m1_parser = subparsers.add_parser('m1', help="solve given data",
                                      description="Solve substance displacement problem with given data")
    m2_parser = subparsers.add_parser('m2', help="generate and solve",
                                      description="Generate test data for substance displacement problem and solve it",
                                      parents=[generator_parser], add_help=False)
    m3_parser = subparsers.add_parser('m3', help="measure time",
                                      description="Generate substance displacement problems with growing sizes and measure solving time")

    m1_parser.add_argument('n', type=int, help="number of substances")
    m1_parser.add_argument('i', nargs='?', type=argparse.FileType('r'), help="input stream with restrictions")

    m3_parser.add_argument('n', type=int, help="starting number of substances")
    m3_parser.add_argument('iterations', type=int, help="number of iterations")
    m3_parser.add_argument('step', type=int, help="increment of substances in each iteration")
    m3_parser.add_argument('density', type=float,
                           help="density of restrictions (0.0 - no restrictions, 1.0 - max restrictions)")
    m3_parser.add_argument('-instances', type=int, help="number of generated problem in each iteration", default=10)

    args = parser.parse_args()

    if args.mode == 'm1':
        args = parse_args_m1(m1_parser, args)

        print(solve_given(args.n, args.i))
    elif args.mode == 'm2':
        if args.type == 'solvable':
            args = generator.parse_args_solvable(m2_parser, args)

            print(generate_and_solve(True, args.n, args.f, args.d))
        elif args.type == 'unsolvable':
            args = generator.parse_args_unsolvable(m2_parser, args)

            print(generate_and_solve(False, args.n, None, args.d))
        else:
            m2_parser.print_usage()
            parser.exit()
    elif args.mode == 'm3':
        args = parse_args_m3(m3_parser, args)

        results = measure(args.n, args.iterations, args.step, args.density, args.instances)
        print('\n'.join('{}\t{}'.format(*result) for result in results))
    else:
        parser.print_usage()
        parser.exit()
