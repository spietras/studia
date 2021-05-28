"""Main script.

This module provides basic CLI entrypoint.

"""
import sys
from contextlib import nullcontext
from pathlib import Path
from typing import Optional

import torch
import typer
from textgen import resource
from textgen.generation import TransformerSentenceIndicesGenerator
from textgen.model.pickers import IndexPickerType, get_picker
from textgen.model.transformer import Transformer

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def main(prompt: Optional[str] = typer.Argument(None, help="Prompt for generation. If None, goes to interactive mode."),
         ckpt_path: Optional[Path] = typer.Option(None, "--cktp", "-c", dir_okay=False, readable=True,
                                                  help="Path to checkpoint path. If None uses bundled one."),
         index_picker: IndexPickerType = typer.Option(IndexPickerType.prob, "--picker", "-p",
                                                      help="Type of index picking")
         ) -> Optional[int]:
    """Command line interface for textgen."""

    with (nullcontext(ckpt_path) if ckpt_path is not None else resource("model.ckpt")) as p:
        system = Transformer.load_from_checkpoint(str(p),
                                                  map_location=torch.device('cpu'),
                                                  index_picker=get_picker(index_picker))

    generator = TransformerSentenceIndicesGenerator(system)
    if prompt is None:
        try:
            import readline  # for nicer prompting
        except ImportError:
            pass  # readline not available
        print('Welcome to text generator! (Ctrl + C or empty text to quit)')
        while prompt := input('Please enter a text: '):
            try:
                print(f"Created sentence: {generator.generate(prompt)}")
            except ValueError as e:
                print(e)
    else:
        try:
            print(generator.generate(prompt))
        except ValueError as e:
            print(e)
            raise typer.Exit(1)


if __name__ == '__main__':
    # entry point for "python -m"
    sys.exit(cli())
