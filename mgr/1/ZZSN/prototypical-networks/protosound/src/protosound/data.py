"""Module for code related to data."""
import csv
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional, Tuple, List, Set, Dict, TypeVar, Hashable, Generic, Iterator, Union

import numpy as np
from pytorch_lightning import LightningDataModule
from scipy import signal
from scipy.io import wavfile
from torch.utils.data import DataLoader, Dataset, IterableDataset, Subset

T = TypeVar('T', bound=Hashable)


class SizedDataset(Dataset, ABC):
    @abstractmethod
    def __len__(self) -> int:
        return NotImplemented


class ClassificationDataset(SizedDataset, Generic[T]):
    @property
    def classes(self) -> Set[T]:
        return set(np.unique(self.targets))

    @property
    @abstractmethod
    def targets(self) -> List[T]:
        return NotImplemented

    @property
    def class_to_ids(self) -> Dict[T, List[int]]:
        target_array = np.array(self.targets)
        return {c: np.where(target_array == c)[0].tolist() for c in self.classes}


class ClassificationDatasetSubset(ClassificationDataset[T]):
    def __init__(self, dataset: ClassificationDataset[T], indices: List[int]) -> None:
        self.dataset = dataset
        self.indices = indices

    @property
    def targets(self) -> List[T]:
        return np.array(self.dataset.targets)[self.indices].tolist()

    def __len__(self) -> int:
        return len(self.indices)

    def __getitem__(self, index: int):
        return self.dataset[self.indices[index]]


class FSD50K(ClassificationDataset[str]):
    """Dataset of FSD50K sound data.

    Item:
        x:
            rate: int - sample rate
            data: (n_samples, n_channels) - sample values for each audio channel
        y: str - target class
    """

    LABEL_FILENAME = "labels.csv"
    AUDIO_DIRNAME = "audio"
    WHITELIST_FILENAME = "whitelist.txt"
    FILE_HEADING = "fname"
    LABELS_HEADING = "labels"

    def __init__(self, base_dir: Union[str, Path], duration: float = 5) -> None:
        super().__init__()

        if isinstance(base_dir, str):
            base_dir = Path(base_dir)

        with open(base_dir / self.WHITELIST_FILENAME) as f:
            whitelist = f.read().splitlines()

        self.items = []
        with open(base_dir / self.LABEL_FILENAME) as f:
            for row in csv.DictReader(f):
                labels = np.array(row[self.LABELS_HEADING].split(","))
                whitelisted_mask = np.array([label in whitelist for label in labels])
                if whitelisted_mask.sum() == 1:  # ignore non-whitelisted and multiclass
                    fname = f"{row[self.FILE_HEADING]}.wav"
                    label = labels[whitelisted_mask][0]
                    self.items.append((fname, label))

        self.base_dir = base_dir
        self.duration = duration

    @property
    def targets(self) -> List[str]:
        return [i[1] for i in self.items]

    def __len__(self) -> int:
        return len(self.items)

    def __getitem__(self, idx: int) -> Tuple[Tuple[int, np.ndarray], str]:
        sample_rate, samples = wavfile.read(self.base_dir / self.AUDIO_DIRNAME / self.items[idx][0])
        samples = np.resize(samples, (self.duration * sample_rate,))
        return (sample_rate, samples), self.items[idx][1]


class FSD50KSpectro(FSD50K):
    """Dataset of FSD50K sound data as spectrograms.

    Item:
        x: (?, ?, ?) - spectrogram array
        y: str - target class
    """

    def __getitem__(self, idx: int) -> Tuple[np.ndarray, str]:
        (rate, samples), y = super().__getitem__(idx)
        _, _, spectrogram = signal.spectrogram(samples, rate)
        return spectrogram[np.newaxis, ...], y


class MinimumSamplesDataset(ClassificationDataset[T]):
    """Dataset wrapper to drop classes that have less than specified number of samples per each class."""

    def __init__(self, base_dataset: ClassificationDataset[T], n_samples: int) -> None:
        super().__init__()
        cti = base_dataset.class_to_ids.copy()
        for c, ids in cti.items():
            if len(ids) < n_samples:
                cti.pop(c)
        self.base_dataset = base_dataset
        self.base_indices = [i for ids in cti.values() for i in ids]

    @property
    def targets(self) -> List[T]:
        return [self.base_dataset.targets[i] for i in self.base_indices]

    def __len__(self) -> int:
        return len(self.base_indices)

    def __getitem__(self, index):
        return self.base_dataset.__getitem__(self.base_indices[index])


class FewShotTasks(IterableDataset):
    """Dataset wrapper for sampling few-shot learning tasks.

    Item:
        support: (n_classes, n_support, *) - support set
        query: (n_classes, n_query, *) - query set
    """

    def __init__(self, base_dataset: ClassificationDataset, n_classes: int, n_support: int, n_query: int) -> None:
        super().__init__()
        self.base_dataset = MinimumSamplesDataset(base_dataset, n_support + n_query)
        self.n_classes = n_classes
        self.n_support = n_support
        self.n_query = n_query

    def __iter__(self) -> Iterator[Tuple[np.ndarray, np.ndarray]]:
        # yield random sampling
        pass  # TODO


class StandardSplit(LightningDataModule):
    """DataModule with data divided into train/val/test sets according to indices."""

    def __init__(self,
                 dataset: Dataset,
                 train_ids: List[int],
                 val_ids: List[int],
                 test_ids: List[int],
                 batch_size: int) -> None:
        super().__init__()
        self.dataset = dataset
        self.train_ids = train_ids
        self.val_ids = val_ids
        self.test_ids = test_ids
        self.batch_size = batch_size

    def setup(self, stage: Optional[str] = None) -> None:
        self.train_set = Subset(self.dataset, self.train_ids)
        self.val_set = Subset(self.dataset, self.val_ids)
        self.test_set = Subset(self.dataset, self.test_ids)

    def train_dataloader(self) -> DataLoader:
        return DataLoader(self.train_set, self.batch_size)

    def val_dataloader(self) -> DataLoader:
        return DataLoader(self.val_set, self.batch_size)

    def test_dataloader(self) -> DataLoader:
        return DataLoader(self.test_set, self.batch_size)


class RandomSplit(StandardSplit):
    """DataModule with data randomly divided into train/val/test sets."""

    def __init__(self, dataset: SizedDataset, batch_size: int,
                 test_ratio: float = 0.15, val_ratio: float = 0.05) -> None:
        train_ids, val_ids, test_ids = self._get_indices(len(dataset), test_ratio, val_ratio)
        super().__init__(dataset, train_ids, val_ids, test_ids, batch_size)

    @staticmethod
    def _get_indices(length: int, test_ratio: float, val_ratio: float) -> Tuple[List[int], List[int], List[int]]:
        cdf = np.cumsum([1 - test_ratio - val_ratio, val_ratio, test_ratio])[:-1]
        stops = (cdf * length).round().astype(int)
        return tuple(x.tolist() for x in np.split(np.random.permutation(length), stops))


class StandardSplitTrainingLimited(StandardSplit):
    """StandardSplit with limited number of examples per class in the training set."""

    def __init__(self, dataset: ClassificationDataset[T], k: int, batch_size: int, val_ratio: float = 0.05) -> None:
        train_ids, val_ids, test_ids = self._get_indices(dataset.targets, k, val_ratio)
        super().__init__(dataset, train_ids, val_ids, test_ids, batch_size)

    @staticmethod
    def _get_indices(targets: List[T], k: int, val_ratio: float) -> Tuple[List[int], List[int], List[int]]:
        targets = np.array(targets)
        classes = np.unique(targets)
        train_ids = [i for c in classes for i in np.random.choice(np.nonzero(targets == c)[0], k, replace=False)]
        free_ids = np.setdiff1d(np.arange(len(targets)), train_ids)
        val_ids = np.random.choice(free_ids, int(val_ratio * len(targets)), replace=False).tolist()
        test_ids = np.setdiff1d(free_ids, val_ids).tolist()
        return train_ids, val_ids, test_ids


def class_split(dataset: ClassificationDataset,
                novel_ratio: float = 0.2) -> tuple[ClassificationDatasetSubset, ClassificationDatasetSubset]:
    """Split dataset into base classes and novel classes."""
    novel_classes = np.random.choice(list(dataset.classes), int(novel_ratio * len(dataset.classes)), replace=False)
    base_classes = np.setdiff1d(list(dataset.classes), novel_classes)
    class_id_map = dataset.class_to_ids
    base_ids = [i for c in base_classes for i in class_id_map[c]]
    novel_ids = [i for c in novel_classes for i in class_id_map[c]]
    return ClassificationDatasetSubset(dataset, base_ids), ClassificationDatasetSubset(dataset, novel_ids)


class FewShotSplit(LightningDataModule):
    def __init__(self,
                 dataset: ClassificationDataset,
                 n_classes: int,
                 n_support: int,
                 n_query: int,
                 test_ratio: float = 0.15,
                 val_ratio: float = 0.05) -> None:
        super().__init__()
        self.dataset = dataset
        self.n_classes = n_classes
        self.n_support = n_support
        self.n_query = n_query
        self.test_ratio = test_ratio
        self.val_ratio = val_ratio

    def setup(self, stage: Optional[str] = None) -> None:
        base, novel = class_split(self.dataset, self.test_ratio)
        train, val = class_split(base, self.val_ratio / (1 - self.test_ratio))
        self.train = FewShotTasks(train, self.n_classes, self.n_support, self.n_query)
        self.val = FewShotTasks(val, self.n_classes, self.n_support, self.n_query)
        self.test = FewShotTasks(novel, self.n_classes, self.n_support, self.n_query)

    def train_dataloader(self) -> DataLoader:
        return DataLoader(self.train)

    def val_dataloader(self) -> DataLoader:
        return DataLoader(self.val)

    def test_dataloader(self) -> DataLoader:
        return DataLoader(self.test)
