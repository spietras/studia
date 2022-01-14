"""Main script.

This module provides basic CLI entrypoint.

"""

import sys
from typing import Optional

import typer
import uvicorn

from retrapi.app import app

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def main(host: str = "0.0.0.0", port: int = 8080) -> Optional[int]:
    """Command line interface for retrapi."""
    uvicorn.run(app, host=host, port=port)


if __name__ == "__main__":
    sys.exit(cli())
