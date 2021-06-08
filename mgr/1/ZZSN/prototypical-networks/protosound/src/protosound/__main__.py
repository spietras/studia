"""Main script.

This module provides basic CLI entrypoint.

"""

import os
import sys
from pathlib import Path
from typing import Optional

import torch
import typer
from protosound.data import FewShotSplit, FSD50KSpectro, RandomSplit, StandardSplitTrainingLimited
from protosound.modules import ProtoSoundFewShotClassifier, SoundEmbedding, SoundClassifier
from pytorch_lightning import Trainer, seed_everything

cli = typer.Typer()  # this is actually callable and thus can be an entry point


@cli.command()
def classic(dataset_path: Path = typer.Argument(..., file_okay=False, readable=True,
                                                help="Path to dataset with train/val/test subdirectories for training"),
            ckpt_path: Path = typer.Option(None, "--ckpt-path", "-c", dir_okay=False, readable=True,
                                           help="Path to checkpoint file on which training will continue"),
            sound_duration: int = typer.Option(5, "--sound-duration", "-d", help="Sound duration in seconds"),
            val_ratio: float = typer.Option(0.2, "--val-ratio", help="Validation set relative size"),
            test_ratio: float = typer.Option(0.2, "--test-ratio", help="Test set relative size"),
            batch_size: int = typer.Option(16, "--batch-size", "-b", help="Batch size"),
            embedding_dim: int = typer.Option(512, "--embedding-dim", help="Embedding dimension"),
            num_workers: int = typer.Option(os.cpu_count(), "--num-workers", "-w", help="Number of Dataloader workers"),
            num_gpus: int = typer.Option(int(torch.cuda.is_available()), "--num-gpus", "-g", help="Number of GPUs"),
            max_epochs: Optional[int] = typer.Option(None, "--max-epochs", "-e",
                                                     help="Maximum number of training epochs"),
            lr: float = typer.Option(0.001, help="Learning rate"),
            seed: Optional[int] = typer.Option(None, "--seed", "-r", help="Seed for reproducibility")
            ):
    if seed is not None:
        seed_everything(seed, workers=True)

    print("Creating dataset...")
    try:
        dataset = FSD50KSpectro(dataset_path, sound_duration)
        datamodule = RandomSplit(dataset,
                                 val_ratio=val_ratio, test_ratio=test_ratio,
                                 batch_size=batch_size, num_workers=num_workers)
        datamodule.setup()  # setup test
    except ValueError as e:
        print(str(e))
        raise typer.Exit(1)

    print("Creating model...")
    if ckpt_path is None:
        model = SoundClassifier(SoundEmbedding(embedding_dim), len(dataset.classes), lr=lr)
    else:
        model = SoundClassifier.load_from_checkpoint(str(ckpt_path))

    trainer = Trainer(max_epochs=max_epochs, gpus=num_gpus,
                      resume_from_checkpoint=ckpt_path, deterministic=seed is not None)

    # Train
    print("Training...")
    trainer.fit(model, datamodule)

    # Test
    trainer.test()


@cli.command()
def limited(dataset_path: Path = typer.Argument(..., file_okay=False, readable=True,
                                                help="Path to dataset with train/val/test subdirectories for training"),
            ckpt_path: Path = typer.Option(None, "--ckpt-path", "-c", dir_okay=False, readable=True,
                                           help="Path to checkpoint file on which training will continue"),
            sound_duration: int = typer.Option(5, "--sound-duration", "-d", help="Sound duration in seconds"),
            limit: int = typer.Option(5, "--limit", "-k", help="Number of training examples per class"),
            batch_size: int = typer.Option(16, "--batch-size", "-b", help="Batch size"),
            embedding_dim: int = typer.Option(512, "--embedding-dim", help="Embedding dimension"),
            num_workers: int = typer.Option(os.cpu_count(), "--num-workers", "-w", help="Number of Dataloader workers"),
            num_gpus: int = typer.Option(int(torch.cuda.is_available()), "--num-gpus", "-g", help="Number of GPUs"),
            max_epochs: Optional[int] = typer.Option(None, "--max-epochs", "-e",
                                                     help="Maximum number of training epochs"),
            lr: float = typer.Option(0.001, help="Learning rate"),
            seed: Optional[int] = typer.Option(None, "--seed", "-r", help="Seed for reproducibility")
            ):
    if seed is not None:
        seed_everything(seed, workers=True)

    print("Creating dataset...")
    try:
        dataset = FSD50KSpectro(dataset_path, sound_duration)
        datamodule = StandardSplitTrainingLimited(dataset, limit, batch_size=batch_size, num_workers=num_workers)
        datamodule.setup()  # setup test
    except ValueError as e:
        print(str(e))
        raise typer.Exit(1)

    print("Creating model...")
    if ckpt_path is None:
        model = SoundClassifier(SoundEmbedding(embedding_dim), len(dataset.classes), lr=lr)
    else:
        model = SoundClassifier.load_from_checkpoint(str(ckpt_path))

    trainer = Trainer(max_epochs=max_epochs, gpus=num_gpus,
                      resume_from_checkpoint=ckpt_path, deterministic=seed is not None)

    # Train
    print("Training...")
    trainer.fit(model, datamodule)

    # Test
    trainer.test()


@cli.command()
def proto(dataset_path: Path = typer.Argument(..., file_okay=False, readable=True,
                                              help="Path to dataset with train/val/test subdirectories for training"),
          ckpt_path: Path = typer.Option(None, "--ckpt-path", "-c", dir_okay=False, readable=True,
                                         help="Path to checkpoint file on which training will continue"),
          sound_duration: int = typer.Option(5, "--sound-duration", "-d", help="Sound duration in seconds"),
          n_classes: int = typer.Option(3, "--n-classes", "-n", help="Number of classes in each task"),
          n_support: int = typer.Option(5, "--n-support", "-k", help="Number of support samples"),
          n_query: int = typer.Option(10, "--n-query", "-q", help="Number of query samples"),
          n_tasks: int = typer.Option(100, "--n-tasks", "-t", help="Number of tasks"),
          val_ratio: float = typer.Option(0.2, "--val-ratio", help="Validation set relative size"),
          test_ratio: float = typer.Option(0.2, "--test-ratio", help="Test set relative size"),
          batch_size: int = typer.Option(1, "--batch-size", "-b", help="Batch size"),
          embedding_dim: int = typer.Option(512, "--embedding-dim", help="Embedding dimension"),
          num_workers: int = typer.Option(os.cpu_count(), "--num-workers", "-w", help="Number of Dataloader workers"),
          num_gpus: int = typer.Option(int(torch.cuda.is_available()), "--num-gpus", "-g", help="Number of GPUs"),
          max_epochs: Optional[int] = typer.Option(None, "--max-epochs", "-e",
                                                   help="Maximum number of training epochs"),
          lr: float = typer.Option(0.001, help="Learning rate"),
          seed: Optional[int] = typer.Option(None, "--seed", "-r", help="Seed for reproducibility")
          ):
    if seed is not None:
        seed_everything(seed, workers=True)

    print("Creating dataset...")
    try:
        datamodule = FewShotSplit(FSD50KSpectro(dataset_path, sound_duration), n_classes, n_support, n_query, n_tasks,
                                  val_ratio=val_ratio, test_ratio=test_ratio,
                                  batch_size=batch_size, num_workers=num_workers)
        datamodule.setup()  # setup test
    except ValueError as e:
        print(str(e))
        raise typer.Exit(1)

    print("Creating model...")
    if ckpt_path is None:
        model = ProtoSoundFewShotClassifier(SoundEmbedding(embedding_dim), lr=lr)
    else:
        model = ProtoSoundFewShotClassifier.load_from_checkpoint(str(ckpt_path))

    trainer = Trainer(max_epochs=max_epochs, gpus=num_gpus,
                      resume_from_checkpoint=ckpt_path, deterministic=seed is not None)

    # Train
    print("Training...")
    trainer.fit(model, datamodule)

    # Test
    trainer.test()


if __name__ == '__main__':
    # entry point for "python -m"
    sys.exit(cli())
