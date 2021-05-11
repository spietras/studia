"""Draw script.

This module provides CLI entrypoint for drawing Sndlib networks.

"""

import sys
from numbers import Real
from pathlib import Path
from typing import Optional

import matplotlib.pyplot as plt
import networkx as nx
import typer
from genetwork.networks import SndlibNetworkXMLParser

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def main(network_path: Path = typer.Argument(...,
                                             readable=True,
                                             help="Path to graph definition file"),
         output_path: Optional[Path] = typer.Option(None,
                                                    "--output", "-o",
                                                    writable=True,
                                                    help="Output path to draw to (if None: drawing to screen)")
         ) -> Optional[int]:
    """Command line interface for drawing Sndlib networks."""

    with open(network_path) as n:
        network = SndlibNetworkXMLParser[str, Real]().parse(n)
    nx.draw(network.graph, with_labels=True)

    if output_path is None:
        plt.show()
    else:
        plt.savefig(output_path)


if __name__ == '__main__':
    # entry point for "python -m"
    sys.exit(cli())
