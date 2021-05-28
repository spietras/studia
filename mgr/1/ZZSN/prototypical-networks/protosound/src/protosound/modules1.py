"""Module for network structures."""

from typing import Tuple

from pytorch_lightning import LightningModule
from torch import Tensor
import torch.nn as nn
import torch.nn.functional as F


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
	self.network=nn.Sequential(nn.Conv2d(3, 32, kernel_size = 3, padding = 1),
            nn.ReLU(),
            nn.Conv2d(32,64, kernel_size = 3, stride = 1, padding = 1),
            nn.ReLU(),
            nn.MaxPool2d(2,2),
            nn.Conv2d(128, 256, kernel_size = 3, stride = 1, padding = 1),
            nn.ReLU(),
            nn.Conv2d(256,256, kernel_size = 3, stride = 1, padding = 1),
            nn.ReLU(),
            nn.MaxPool2d(2,2),
            
            nn.Flatten()
)

    def forward(self, x: Tensor) -> Tensor:
        pass  # TODO
        return self.network(x)
    
    def training_step(self, batch, batch_idx):
	x, y = batch
	y_hat = self(x)
	loss = F.cross_entropy(y_hat, y)
	return loss
    
    def configure_optimizers(self):
        return torch.optim.Adam(self.parameters(), lr=0.02)


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
	self.network = nn.Sequential(
	    nn.Linear(embedding.embedding_dim,1024),
            nn.ReLU(),
            nn.Linear(1024, 512),
            nn.ReLU(),
            nn.Linear(512,n_classes))

    def forward(self, x: Tensor) -> Tensor:
        pass  # TODO
	return self.network(x)

    def training_step(self, batch: Tuple[Tensor, Tensor], batch_idx: int) -> Tensor:
        x, y = batch
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
