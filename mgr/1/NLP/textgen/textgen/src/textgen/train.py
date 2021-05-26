"""Main script.

This module provides basic CLI entrypoint.

"""
import os
import pickle
import sys
from pathlib import Path
from typing import Optional

import torch
import typer
from pytorch_lightning import Trainer
from textgen.data.modules import SentenceCompletionIterableSplitFromDir
from textgen.model.transformer import Transformer, TransformerModel, TransformerProbabilisticGenerator

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def train(dataset_path: Path = typer.Argument(..., help="Path to dataset with train/val/test subdirectories for "
                                                        "training."),
          ckpt_path: Path = typer.Option(None, help="Path to checkpoint file on which training will continue."),
          max_length: int = typer.Option(20, help="Maximum sentence length in words."),
          batch_size: int = typer.Option(64, help="Batch size."),
          num_workers: int = typer.Option(os.cpu_count(), help="Number of Dataloader workers"),
          num_gpus: int = typer.Option(int(torch.cuda.is_available()), help="Number of GPUs."),
          max_epochs: int = typer.Option(30, help="Maximum number of training epochs."),
          lr: float = typer.Option(0.01, help="Learning rate."),
          d_model: int = typer.Option(64, help="The size of the vector representing one token."),
          d_ff: int = typer.Option(16, help="The size of the first layer in the FFN in each layer of the "
                                            "encoder/decoder stack."),
          num_heads: int = typer.Option(4, help="The number of attention heads."),
          num_layers: int = typer.Option(4, help="The number of encoder/decoder stack layers."),
          drop_out_rate: float = typer.Option(0.1, help="Dropout rate.")) -> Optional[int]:
    """Command line interface for textgen-train."""

    datamodule = SentenceCompletionIterableSplitFromDir(dataset_path, max_length=max_length, batch_size=batch_size,
                                                        num_workers=num_workers)
    vocab_size = len(datamodule.config.corpus.vocabulary)

    # Save configuration
    with open('resources/config.pickle', 'wb') as handle:
        pickle.dump(datamodule.config, handle)

    model = TransformerModel(vocab_size, vocab_size, d_model, d_ff, num_heads, num_layers, drop_out_rate, max_length)
    generator = TransformerProbabilisticGenerator(datamodule.config)
    system = Transformer(model, generator, lr=lr)

    trainer = Trainer(max_epochs=max_epochs, gpus=num_gpus, resume_from_checkpoint=ckpt_path)

    # Train
    trainer.fit(system, datamodule)

    # Test
    trainer.test()


if __name__ == '__main__':
    # entry point for "python -m"
    sys.exit(cli())
