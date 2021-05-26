"""Main script.

This module provides basic CLI entrypoint.

"""
import pickle
import sys
from pathlib import Path
from typing import Optional

import typer
from textgen.generation import TransformerSentenceIndicesGenerator
from textgen.model.transformer import Transformer, TransformerProbabilisticGenerator, \
    TransformerGreedyGenerator

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def main(ckpt_path: Path = typer.Argument(..., help="Path to checkpoint path."),
         config_path: Path = typer.Argument(..., help="Path to configuration file created during training."),
         gen_type: str = typer.Option("probabilistic", help="The type of generator. Available are: probabilistic and "
                                                            "greedy.")) -> Optional[int]:
    """Command line interface for textgen."""

    with open(config_path, 'rb') as handle:
        config = pickle.load(handle)

    system = Transformer.load_from_checkpoint(str(ckpt_path))
    if gen_type == "greedy":
        trans_gen = TransformerGreedyGenerator(config)
    else:
        trans_gen = TransformerProbabilisticGenerator(config)
    generator = TransformerSentenceIndicesGenerator(system.model, trans_gen)

    print('Welcome to text generator! (ctrl + c to quit)')
    while text := input('Please enter a text: '):
        print(f"Created sentence: {generator.generate(text, 'cpu')}")


if __name__ == '__main__':
    # entry point for "python -m"
    sys.exit(cli())
