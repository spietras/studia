"""Module for network structures."""

from typing import Tuple

from pytorch_lightning import LightningModule
from torch import Tensor


class SoundEmbedding(LightningModule):
    """Convolutional model for making sound embeddings.

    Sound should be in form of a spectrogram.

    Shape:
        input: (batch_size, ?, ?, ?)
        output: (batch_size, embedding_dim)
    """

    def __init__(self, embedding_dim: int) -> None:
        super().__init__()
        self.embedding_dim = embedding_dim

    def forward(self, x: Tensor) -> Tensor:
        pass  # TODO


class SoundClassifier(LightningModule):
    """Traditional sound classifier with embedding, projection, softmax and cross-entropy loss.

    Sound should be in form of a spectrogram.
    Returns probabilities of sample belonging to each class.

    Shape:
        input: (batch_size, ?, ?, ?)
        output: (batch_size, n_classes)
    """

    def __init__(self, embedding: SoundEmbedding, n_classes: int) -> None:
        super().__init__()
        self.embedding = embedding
        self.n_classes = n_classes

    def forward(self, x: Tensor) -> Tensor:
        pass  # TODO

    def training_step(self, batch: Tuple[Tensor, Tensor], batch_idx: int) -> Tensor:
        x, y = batch
        pass  # TODO


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

    During training

    Shape (inference):
        input: (batch_size, n_way, k_shot, ?, ?, ?),
               (batch_size, n_query, ?, ?, ?)
        output: (batch_size, n_query, n_way)
    Shape (train/val/test):
        input: (batch_size, n_way, k_shot, ?, ?, ?),
               (batch_size, n_way, n_query, ?, ?, ?)
    """

    def forward(self, support: Tensor, query: Tensor) -> Tensor:
        pass  # TODO

    def training_step(self, batch: Tuple[Tensor, Tensor], batch_idx: int) -> Tensor:
        support, query = batch
        pass  # TODO
