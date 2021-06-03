"""Module for network structures."""

from typing import Tuple

import torch.nn as nn
import torch.nn.functional as F
from pytorch_lightning import LightningModule
from torch import Tensor


class SoundEmbedding(nn.Module):
    """Convolutional model for making sound embeddings.

    Sound should be in form of a spectrogram.

    Shape:
        input: (batch_size, 1, ?, ?)
        output: (batch_size, embedding_dim)
    """

    def __init__(self, embedding_dim: int) -> None:
        super().__init__()
        self.embedding_dim = embedding_dim
        self.network = nn.Sequential(nn.Conv2d(1, 32, kernel_size=(3,), padding=(1,)),
                                     nn.ReLU(),
                                     nn.Conv2d(32, 64, kernel_size=(3,), stride=(1,), padding=(1,)),
                                     nn.ReLU(),
                                     nn.MaxPool2d(2, 2),
                                     nn.Conv2d(64, 128, kernel_size=(3,), stride=(1,), padding=(1,)),
                                     nn.ReLU(),
                                     nn.Conv2d(128, 128, kernel_size=(3,), stride=(1,), padding=(1,)),
                                     nn.ReLU(),
                                     nn.MaxPool2d(2, 2),
                                     nn.Conv2d(128, 256, kernel_size=(3,), stride=(1,), padding=(1,)),
                                     nn.ReLU(),
                                     nn.Conv2d(256, embedding_dim, kernel_size=(3,), stride=(1,), padding=(1,)),
                                     nn.ReLU(),
                                     nn.MaxPool2d(2, 2),
                                     nn.AdaptiveAvgPool2d(1),
                                     nn.Flatten())

    def forward(self, x: Tensor) -> Tensor:
        return self.network(x)


class SoundClassifier(LightningModule):
    """Traditional sound classifier with embedding, projection, softmax and cross-entropy loss.

    Sound should be in form of a spectrogram.
    Returns probabilities of sample belonging to each class.

    Shape:
        input: (batch_size, 1, ?, ?)
        output: (batch_size, n_classes)
    """

    def __init__(self, embedding: SoundEmbedding, n_classes: int) -> None:
        super().__init__()
        self.embedding = embedding
        self.n_classes = n_classes
        self.network = nn.Sequential(
            nn.Linear(embedding.embedding_dim, n_classes),
            nn.Softmax(1)
        )

    def forward(self, x: Tensor) -> Tensor:
        return self.network(x)

    def training_step(self, batch: Tuple[Tensor, Tensor], batch_idx: int) -> Tensor:
        pass  # TODO

    def training_step(self, batch, batch_idx):
        x, y = batch
        y_hat = self(x)
        loss = F.cross_entropy(y_hat, y)
        return loss

    def configure_optimizers(self):
        return torch.optim.Adam(self.parameters(), lr=0.02)


class SoundClassifierWithTransfer(SoundClassifier):
    """SoundClassifier with frozen embedding."""

    def __init__(self, embedding: SoundEmbedding, n_classes: int) -> None:
        embedding.freeze()
        super().__init__(embedding, n_classes)


class ProtoSoundFewShotClassifier(LightningModule):
    """Sound classifier supported by prototypical networks method.

    Sound should be in form of a spectrogram.
    Returns probabilities of sample belonging to each class in support set.

    During inference you need to provide support set with n classes and k samples (n-way k-shot)
    and query set with the same classes as support set. In other words: the point of this method
    is to properly classify samples belonging to unseen before classes, given small a number of
    samples in each class (during inference) as a basis for classification.

    Shape (inference):
        input: (batch_size, n_way, k_shot, 1, ?, ?),
               (batch_size, n_query, 1, ?, ?)
        output: (batch_size, n_query, n_way)
    Shape (train/val/test):
        input: (batch_size, n_way, k_shot, 1, ?, ?),
               (batch_size, n_way, n_query, 1, ?, ?)
    """

    def forward(self, support: Tensor, query: Tensor) -> Tensor:
        pass  # TODO

    def training_step(self, batch: Tuple[Tensor, Tensor], batch_idx: int) -> Tensor:
        support, query = batch
        pass  # TODO
