"""Main script.

This module provides basic CLI entrypoint.

"""

import sys
from numbers import Real
from pathlib import Path
from typing import Optional

import typer
from genetwork.networks import SndlibNetworkXMLParser

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def main(network_path: Path = typer.Argument(...,
                                             readable=True,
                                             help="Path to graph definition file")) -> Optional[int]:
    """Command line interface for genetwork."""

    with open(network_path) as n:
        network = SndlibNetworkXMLParser[str, Real]().parse(n)
    typer.echo(str(network))


if __name__ == '__main__':
    # entry point for "python -m"
    sys.exit(cli())
