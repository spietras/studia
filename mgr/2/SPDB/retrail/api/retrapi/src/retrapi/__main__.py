"""Main script.

This module provides basic CLI entrypoint.

"""

import sys
from typing import Optional

import typer
import uvicorn

from retrapi.app import app
from retrapi.models.config import DatabaseConfig

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def main() -> Optional[int]:
    """Command line interface for retrapi."""

    config = DatabaseConfig()
    typer.echo(config.uri)

    uvicorn.run(app, host="0.0.0.0", port=8080)


if __name__ == '__main__':
    sys.exit(cli())
