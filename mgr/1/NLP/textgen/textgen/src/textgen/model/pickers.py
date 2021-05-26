from abc import ABC, abstractmethod
from enum import Enum

import torch
from torch import Tensor


class IndexPicker(ABC):
    """Base class for index picking methods."""

    @abstractmethod
    def pick_index(self, predictions: Tensor) -> Tensor:
        return NotImplemented


class GreedyIndexPicker(IndexPicker):
    """IndexPicker that always picks the most probable index."""

    def pick_index(self, predictions: Tensor) -> Tensor:
        return predictions.argmax()


class ProbabilisticIndexPicker(IndexPicker):
    """IndexPicker that picks index based on probability distribution."""

    def pick_index(self, predictions: Tensor) -> Tensor:
        return torch.multinomial(predictions.exp(), 1)[0]


class IndexPickerType(str, Enum):
    greedy = "greedy"
    prob = "prob"


def get_picker(type: IndexPickerType, *args, **kwargs) -> IndexPicker:
    return {IndexPickerType.greedy: GreedyIndexPicker,
            IndexPickerType.prob: ProbabilisticIndexPicker}[type](*args, **kwargs)
