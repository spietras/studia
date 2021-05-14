"""Main script.

This module provides basic CLI entrypoint.

"""
import json
import sys
from numbers import Real
from pathlib import Path
from typing import Optional

import typer
from genetwork.genetic.crossovers import SinglePointRandomCrossover
from genetwork.genetic.evaluators import TotalCostWithConstraintsSndlibEvaluator
from genetwork.genetic.evolution import Evolution
from genetwork.genetic.initializers import RandomSndlibPopulationInitializer
from genetwork.genetic.mutations import DeltaMutation
from genetwork.genetic.selections import ThresholdSelection
from genetwork.genetic.successions import BestOverallSuccession
from genetwork.networks import SndlibNetworkXMLParser, SndlibTransponderType

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def main(network_path: Path = typer.Argument(...,
                                             help="Path to graph definition file"),
         max_iters: int = typer.Option(default=1000,
                                       help="Maximum iterations"),
         pop_size: int = typer.Option(default=500,
                                      help="Initial population size"),
         selection_threshold: int = typer.Option(default=250,
                                                 help="Threshold of best individuals that will participate in selection"),
         selection_size: int = typer.Option(default=250,
                                            help="The number of individuals that will be selected for crossing and mutation"),
         pm: float = typer.Option(default=0.1,
                                  help="Probability of mutation"),
         pc: float = typer.Option(default=0.75,
                                  help="Probability of crossover"),
         alpha: float = typer.Option(default=100,
                                     help="Alpha parameter of the objective function (unnecessary overcapacity)"),
         beta: float = typer.Option(default=100,
                                    help="Beta parameter of the objective function (exceeding fiber capacity)"),
         lam: int = typer.Option(default=32,
                                 help="Maximum capacity of wavelengths (lambdas) in a single fiber"),
         config_path: Path = typer.Option(default="genetwork/src/genetwork/resources/config.json",
                                          help="Path to configuration file")) -> Optional[int]:
    """Command line interface for genetwork."""

    with open(network_path) as n:
        network = SndlibNetworkXMLParser[str, Real]().parse(n)
    with open(config_path) as c:
        config = json.load(c)

    transponders = []
    for t in config["transponders"]:
        transponders.append(SndlibTransponderType(t["capacity"], t["cost"]))

    initializer = RandomSndlibPopulationInitializer(pop_size, transponders, network.demands)
    evaluator = TotalCostWithConstraintsSndlibEvaluator(network.demands, alpha, beta, lam)
    selection = ThresholdSelection(evaluator, selection_threshold, selection_size)
    crossover = SinglePointRandomCrossover(pc)
    mutation = DeltaMutation(pm)
    succession = BestOverallSuccession(evaluator)

    evolution = Evolution(initializer, evaluator, selection, crossover, mutation, succession)

    for i in range(0, max_iters):
        print(f"Evolution: {i}")
        evolution.show_best_info()
        evolution.step()


if __name__ == '__main__':
    # entry point for "python -m"
    sys.exit(cli())
