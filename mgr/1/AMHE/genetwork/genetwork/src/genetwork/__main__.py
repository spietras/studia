"""Main script.

This module provides basic CLI entrypoint.

"""
import json
import sys
from contextlib import nullcontext
from pathlib import Path
from typing import Optional, Dict

import typer
from genetwork import resource
from genetwork.genetic.creatures import SndlibGene
from genetwork.genetic.crossovers import SinglePointRandomCrossover
from genetwork.genetic.evaluators import TotalCostWithConstraintsSndlibEvaluator
from genetwork.genetic.evolution import Evolution, NoChangeStoppingCriterion, MaxStepsStoppingCriterion
from genetwork.genetic.initializers import RandomSndlibPopulationInitializer
from genetwork.genetic.mutations import DeltaMutation
from genetwork.genetic.selections import ThresholdSelection
from genetwork.genetic.successions import BestOverallSuccession
from genetwork.log import EvolutionConsoleLogger
from genetwork.networks import SndlibNetworkXMLParser, SndlibTransponderType, SndlibNode, SndlibNetwork

cli = typer.Typer()  # this is actually callable and thus can be an entry point


def get_config_dict(path: Optional[Path]) -> Dict:
    ctx = nullcontext() if path is not None else resource("config.json")
    with ctx as r:
        with open(r) as f:
            return json.load(f)


def get_network(path: Path, max_paths: int) -> SndlibNetwork:
    with open(path) as n:
        return SndlibNetworkXMLParser[SndlibNode, float](max_paths).parse(n)


def get_output(evolution: Evolution[SndlibGene], network: SndlibNetwork) -> str:
    best = min(evolution.population, key=lambda x: evolution.evaluator.evaluate_creature(x))
    score, valid = evolution.evaluator.evaluate(best)
    out = {"score": score, "valid": valid, "demands": []}
    for gene, demand in zip(best.chromosome, network.demands):
        demand_dict = {"demand": {"source": demand.source.serialize(), "target": demand.target.serialize()},
                       "placements": []}
        for placements in gene.placements:
            placement_dict = {"path": placements.path.serialize(),
                              "transponders": {t.capacity: q for t, q in placements.quantity_dict.items()}}
            demand_dict["placements"].append(placement_dict)
        out["demands"].append(demand_dict)
    return json.dumps(out, indent=2)


@cli.command()
def main(network_path: Path = typer.Argument(..., dir_okay=False, readable=True, help="Path to graph definition file"),
         max_iters: Optional[int] = typer.Option(None, "--max-iters", "-i",
                                                 help="Maximum iterations, if None stops when no changes occur"),
         pop_size: int = typer.Option(500, "--pop-size", "-p", help="Initial population size"),
         selection_threshold: float = typer.Option(0.75, "--selection-threshold", "-t",
                                                   help="Percent of population that will participate in selection"),
         selection_factor: float = typer.Option(2, "--selection-factor", "-f",
                                                help="Relative size of selected population"),
         pm: float = typer.Option(0.002, "--pm", "-m", help="Probability of mutation"),
         pc: float = typer.Option(0.99, "--pc", "-c", help="Probability of crossover"),
         alpha: float = typer.Option(25, "--alpha", "-a",
                                     help="Alpha parameter of the objective function (undersupplying)"),
         beta: float = typer.Option(7.5, "--beta", "-b",
                                    help="Beta parameter of the objective function (exceeding fiber capacity)"),
         lam: int = typer.Option(96, "--lam", "-l", help="Maximum capacity of wavelengths (lambdas) in a single fiber"),
         max_paths: int = typer.Option(4, "--max-paths", "-P",
                                       help="Maximum paths to generate if there are no admissiblePaths in XML"),
         max_trans: int = typer.Option(1, "--max-trans", "-T",
                                       help="Maximum number of transponder on a path that can be drawn during "
                                            "initialization"),
         succ_prob: float = typer.Option(0.9, "--succ-prob", "-s",
                                         help="Success probability used in geometric distribution while drawing "
                                              "the number of transponders on a path"),
         config_path: Optional[Path] = typer.Option(None, "--config-path", "-c", dir_okay=False, readable=True,
                                                    help="Path to configuration file"),
         out_path: Optional[Path] = typer.Option(None, "--out-path", "-o", writable=True,
                                                 help="Output file path, if None prints to stdout")
         ) -> Optional[int]:
    """Command line interface for genetwork."""

    config = {"max_iters": max_iters, "pop_size": pop_size, "selection_threshold": selection_threshold,
              "selection_factor": selection_factor, "pm": pm, "pc": pc, "alpha": alpha, "beta": beta, "lam": lam}
    config.update(get_config_dict(config_path))

    network = get_network(network_path, max_paths)

    transponders = [SndlibTransponderType(t["capacity"], t["cost"]) for t in config["transponders"]]

    initializer = RandomSndlibPopulationInitializer(config["pop_size"], transponders, network.demands,
                                                    max_trans, succ_prob)
    evaluator = TotalCostWithConstraintsSndlibEvaluator(network.demands, config["alpha"], config["beta"], config["lam"])
    selection = ThresholdSelection(evaluator, config["selection_threshold"], config["selection_factor"])
    crossover = SinglePointRandomCrossover(config["pc"])
    mutation = DeltaMutation(config["pm"])
    succession = BestOverallSuccession(evaluator)

    evolution = Evolution[SndlibGene](initializer, evaluator, selection, crossover, mutation, succession)
    max_iter = config["max_iters"]
    stop_criterion = NoChangeStoppingCriterion() if max_iter is None else MaxStepsStoppingCriterion(max_iter)
    logger = EvolutionConsoleLogger()

    while not stop_criterion.should_stop(evolution):
        evolution.step()
        logger.log(evolution)

    out = get_output(evolution, network)
    if out_path is None:
        print(out)
    else:
        out_path.write_text(out)


if __name__ == '__main__':
    # entry point for "python -m"
    sys.exit(cli())
