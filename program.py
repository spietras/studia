"""
Main project script

Author: Sebastian Pietras
Project: Chemikalia
"""

import argparse
import statistics
import sys

import algorithm
import generator
import measuring


def solve_given(n, restrictions):
    """
    Solve problem given restrictions

    Parameters:
        n - number of substances
        restrictions - list of restrictions
    """
    return algorithm.solve(n, restrictions)


def generate_and_solve(solvable, n, first_n, density):
    """
    Generate problem and solve it

    Parameters:
        solvable - should generated problem be solvable
        n - number of substances
        first_n - number of substances in one of the magazines (ignored if unsolvable)
        density - density of restrictions
    """
    return algorithm.solve(n, generator.generate(solvable, n, first_n, density))


def measure(start_n, iterations, step, density, instances, repeats_per_instance=5, verbosity=0):
    """
    Measure solving time of problems with different sizes

    Parameters:
        start_n - starting number of substances
        iterations - number of times to increase problem size
        step - how much to increase number of substances in each iteration
        density - density of restrictions
        instances - how many problems to generate in each iteration
        repeats_per_instance - how many times to repeat each problem
        verbosity - 0 prints nothing, 1 prints header with basic info, 2 prints info after each iteration
    """
    if verbosity >= 1:
        print(
            "Measuring for density {}, starting with n = {}, with step {}, {} iterations, {} instances per iterations and {} repeats per instance".format(
                density, start_n, step, iterations, instances, repeats_per_instance))

    points = [n for n in range(start_n, start_n + iterations * step, step)]
    times = []
    for i, n in enumerate(points):
        def algorithm_fun(edges, v=n):
            algorithm.solve(v, edges)

        def generator_fun(v=n, d=density):
            return generator.generate(True, v, int(v / 2), d)

        times.append(measuring.measure_generated(algorithm_fun, generator_fun, instances, repeats_per_instance))
        if verbosity >= 2:
            print("Measured for n = {} ({}/{})".format(n, i + 1, iterations))

    if verbosity >= 1:
        print("Measured all")

    return list(zip(points, times))


def parse_args_m1(parser, args):
    if not args.n >= 1:
        parser.error("Minimum substance number is 1")

    restrictions = []
    with open(args.i, 'r') if args.i is not None else sys.stdin as f:
        for line in f:
            substances_string_list = line.split()

            if substances_string_list:
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
    if args.instances is not None and not args.instances >= 1:
        parser.error("Minimum repeats number is 1")
    if args.verbose > 2:
        args.verbose = 2

    return args


def print_results(results):
    bad_edge = results[0]
    warehouse_a = results[1]
    warehouse_b = results[2]

    if bad_edge:
        print("Can't place substances. Problematic restriction: ", bad_edge)
        print("Current warehouse A: " + ' '.join(str(substance) for substance in warehouse_a))
        print("Current warehouse B: " + ' '.join(str(substance) for substance in warehouse_b))
        return

    print("Warehouse A: " + ' '.join(str(substance) for substance in warehouse_a))
    print("Warehouse B: " + ' '.join(str(substance) for substance in warehouse_b))
    return


def print_table(results, density):
    points, times = map(list, zip(*results))
    median_point = statistics.median(points)
    median_time = statistics.median(times)
    qs = list()

    for point, time in results:
        q = (time * algorithm.T(median_point, density)) / (algorithm.T(point, density) * median_time)
        qs.append(q)

    print("\nAlgorytm z asymptotą O(n + m) dla m = {} * n^2 / 4\n".format(density))
    print('{:<10s}\t{:<10s}\t{:<10s}'.format("n", "t(n)[ms]", "q(n)"))
    print('\n'.join(
        '{:<10d}\t{:<10.5f}\t{:<10.5f}'.format(point, time, q) for point, time, q in zip(points, times, qs)))
    return


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
    m3_parser.add_argument('-ins', '--instances', type=int, help="number of generated problem in each iteration",
                           default=10)
    m3_parser.add_argument('-r', '--repeats', type=int,
                           help="number of repeats of each generated problem in each iteration",
                           default=10)
    m3_parser.add_argument('-v', '--verbose', action='count', help="verbosity level, count flag", default=0)

    args = parser.parse_args()

    if args.mode == 'm1':
        args = parse_args_m1(m1_parser, args)

        print_results(solve_given(args.n, args.i))
    elif args.mode == 'm2':
        if args.type == 'solvable':
            args = generator.parse_args_solvable(m2_parser, args)

            print_results(generate_and_solve(True, args.n, args.first, args.density))
        elif args.type == 'unsolvable':
            args = generator.parse_args_unsolvable(m2_parser, args)

            print_results(generate_and_solve(False, args.n, None, args.density))
        else:
            m2_parser.print_usage()
            parser.exit()
    elif args.mode == 'm3':
        args = parse_args_m3(m3_parser, args)

        results = measure(args.n, args.iterations, args.step, args.density, args.instances, args.repeats, args.verbose)

        print_table(results, args.density)
    else:
        parser.print_usage()
        parser.exit()
