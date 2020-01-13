import timeit


def measure(algorithm_fun, n_repeats=1):
    return 1000.0 * timeit.timeit(algorithm_fun, number=n_repeats) / n_repeats


def measure_generated(algorithm_fun, generator_fun, outer_repeats=1, inner_repeats=1):
    times_sum = 0.0
    for _ in range(outer_repeats):
        def measured_fun(data=generator_fun()): algorithm_fun(data)

        times_sum += measure(measured_fun, inner_repeats)
    return times_sum / outer_repeats
