"""Main script.

This module provides basic CLI entrypoint.

"""
import os
import sys
from pathlib import Path
from typing import Optional

import torch
import typer
from pytorch_lightning import Trainer, seed_everything
from textgen.data.modules import SentenceCompletionIterableSplitFromDir
from textgen.model.pickers import IndexPickerType, get_picker
from textgen.model.transformer import Transformer, TransformerModel

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def train(dataset_path: Path = typer.Argument(..., file_okay=False, readable=True,
                                              help="Path to dataset with train/val/test subdirectories for training."),
          ckpt_path: Path = typer.Option(None, "--ckpt-path", "-c", dir_okay=False, readable=True,
                                         help="Path to checkpoint file on which training will continue."),
          max_length: int = typer.Option(20, "--max-length", "-s", help="Maximum sentence length in words."),
          batch_size: int = typer.Option(64, "--batch-size", "-b", help="Batch size."),
          num_workers: int = typer.Option(os.cpu_count(), "--num-workers", "-w", help="Number of Dataloader workers"),
          num_gpus: int = typer.Option(int(torch.cuda.is_available()), "--num-gpus", "-g", help="Number of GPUs."),
          max_epochs: int = typer.Option(30, "--max-epochs", "-e", help="Maximum number of training epochs."),
          lr: float = typer.Option(0.01, help="Learning rate."),
          d_model: int = typer.Option(64, "--d-model", "-m", help="The size of the vector representing one token."),
          d_ff: int = typer.Option(16, "--d-ff", "-f", help="The size of the first layer in the FFN in each layer "
                                                            "of the encoder/decoder stack."),
          num_heads: int = typer.Option(4, "--num-heads", "-h", help="The number of attention heads."),
          num_layers: int = typer.Option(4, "--num-layers", "-l", help="The number of encoder/decoder stack layers."),
          drop_out_rate: float = typer.Option(0.1, "--dropout-rate", "-d", help="Dropout rate."),
          index_picker: IndexPickerType = typer.Option(IndexPickerType.greedy, "--index-picker", "-p",
                                                       help="Type of index picking"),
          teacher_forcing_val: bool = typer.Option(False, "--teacher-forcing-val", "-t",
                                                   help="Use teacher forcing in validation and test stages"),
          seed: Optional[int] = typer.Option(None, "--seed", "-r", help="Seed for reproducibility")
          ) -> Optional[int]:
    """Command line interface for textgen-train."""

    if seed is not None:
        seed_everything(seed, workers=True)

    print("Evaluating dataset...")
    datamodule = SentenceCompletionIterableSplitFromDir(dataset_path, max_length=max_length, batch_size=batch_size,
                                                        num_workers=num_workers)

    print("Creating model...")
    if ckpt_path is None:
        vocab_size = len(datamodule.config.corpus.vocabulary)
        model = TransformerModel(vocab_size, vocab_size, d_model, d_ff, num_heads, num_layers, drop_out_rate,
                                 max_length)
        system = Transformer(model, datamodule.config, get_picker(index_picker),
                             lr=lr, teacher_forcing_val=teacher_forcing_val)
    else:
        system = Transformer.load_from_checkpoint(str(ckpt_path))

    trainer = Trainer(max_epochs=max_epochs, gpus=num_gpus,
                      resume_from_checkpoint=ckpt_path, deterministic=seed is not None)

    # Train
    print("Training...")
    trainer.fit(system, datamodule)

    # Test
    trainer.test()


if __name__ == '__main__':
    # entry point for "python -m"
    sys.exit(cli())
