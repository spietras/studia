import timeit


def measure(algorithm_lambda, n_repeats=1):
    return timeit.timeit(lambda: algorithm_lambda(), number=n_repeats)