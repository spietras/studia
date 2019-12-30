import timeit


def measure(algorithm_lambda, n_repeats=1):
    return timeit.timeit(algorithm_lambda, number=n_repeats)
