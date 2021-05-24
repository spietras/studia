"""Module for network structures."""

from typing import Tuple, Optional, Callable, Dict

import torch
import torch.nn.functional as F
from pytorch_lightning import LightningModule
from torch import Tensor, nn
from torch.nn import NLLLoss
from torchmetrics import Accuracy


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


class ProtoFewShotClassifier(LightningModule):
    """Classifier supported by prototypical networks method.

    Returns log probabilities of sample belonging to each class in support set.

    During inference you need to provide support set with n classes and k samples (n-way k-shot)
    and query set with the same classes as support set. In other words: the point of this method
    is to properly classify samples belonging to unseen before classes, given small a number of
    samples in each class (during inference) as a basis for classification.

    During training

    Shape (inference):
        input: (batch_size, n_way, k_shot, *),
               (batch_size, n_query, *)
        output: (batch_size, n_query, n_way)
    Shape (train/val/test):
        input: (batch_size, n_way, k_shot, *),
               (batch_size, n_query, *),
               (batch_size, n_query)
    """

    def __init__(self, embedding: nn.Module,
                 loss: Optional[Callable[[Tensor, Tensor], Tensor]] = None,
                 metric: Optional[Callable[[Tensor, Tensor], Tensor]] = None) -> None:
        super().__init__()
        self.embedding = embedding
        self.loss = loss or NLLLoss()
        self.metric = metric or Accuracy()

    def forward(self, support: Tensor, query: Tensor) -> Tensor:
        (batch_size, n_way, k_shot), n_query = support.shape[:3], query.shape[1]
        support = self.embedding(support.flatten(0, 2)).unflatten(0, batch_size, n_way, k_shot)  # (B, N, K, Z)
        query = self.embedding(query.flatten(0, 1)).unflatten(0, (batch_size, n_query))  # (B, Q, Z)
        prototypes = support.mean(2)  # (B, N, Z)
        return F.log_softmax(-torch.cdist(query, prototypes), -1)  # (B, Q, N)

    def training_step(self, batch: Tuple[Tensor, Tensor, Tensor], batch_idx: int) -> Tensor:
        support, query, y = batch
        pred = self(support, query)
        return self.loss(pred, y)

    def _metric_step(self, support: Tensor, query: Tensor, y: Tensor,
                     metric_label: str, loss_label: str) -> Dict[str, Tensor]:
        pred = self(support, query)
        loss = self.loss(pred, y)
        metric = self.metric(pred.exp(), y)  # accuracy needs normal probabilities
        metrics = {metric_label: metric, loss_label: loss}
        self.log_dict(metrics)
        return metrics

    def validation_step(self, batch: Tuple[Tensor, Tensor, Tensor], batch_idx: int) -> Dict[str, Tensor]:
        support, query, y = batch
        return self._metric_step(support, query, y, 'val_metric', 'val_loss')

    def test_step(self, batch: Tuple[Tensor, Tensor, Tensor], batch_idx: int) -> Dict[str, Tensor]:
        support, query, y = batch
        return self._metric_step(support, query, y, 'test_metric', 'test_loss')


class ProtoSoundFewShotClassifier(ProtoFewShotClassifier):
    """ProtoFewShotClassifier with SoundEmbedding for sound classification."""

    def __init__(self, embedding: SoundEmbedding,
                 loss: Optional[Callable[[Tensor, Tensor], Tensor]] = None,
                 metric: Optional[Callable[[Tensor, Tensor], Tensor]] = None) -> None:
        super().__init__(embedding, loss, metric)
