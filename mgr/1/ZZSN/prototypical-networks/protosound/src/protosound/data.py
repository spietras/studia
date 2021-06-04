"""Module for code related to data."""

import csv
import random
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional, Tuple, List, Set, Dict, TypeVar, Hashable, Generic, Iterator, Union, Any

import librosa
import numpy as np
from pytorch_lightning import LightningDataModule
from torch.utils.data import DataLoader, Dataset, IterableDataset, Subset

T = TypeVar('T', bound=Hashable)


class SizedDataset(Dataset, ABC):
    """Base class for datasets with __len__."""

    @abstractmethod
    def __len__(self) -> int:
        return NotImplemented


class SizedIterableDataset(IterableDataset, ABC):
    """Base class for iterable datasets with __len__."""

    @abstractmethod
    def __len__(self) -> int:
        return NotImplemented


class ClassificationDataset(SizedDataset, Generic[T]):
    """Base class for classification datasets with single class for each item."""

    def __init__(self, targets: List[T]) -> None:
        super().__init__()
        self._class_encoding = {t: i for i, t in enumerate(list(set(targets)))}
        self._targets = [self._class_encoding[t] for t in targets]

    @property
    def class_encoding(self) -> Dict[T, int]:
        """Map from real class value to encoded int representation."""
        return self._class_encoding

    @property
    def reverse_class_encoding(self) -> Dict[int, T]:
        """Map from encoded classes to real values."""
        return {i: t for t, i in self.class_encoding.items()}

    @property
    def classes(self) -> Set[int]:
        """All available classes (encoded)."""
        return set(self.class_encoding.values())

    @property
    def targets(self) -> List[int]:
        """Targets of all items (encoded)."""
        return self._targets

    @property
    def class_to_ids(self) -> Dict[int, List[int]]:
        """Map from encoded class to indices of all items of that class."""
        target_array = np.array(self.targets)
        return {c: np.where(target_array == c)[0].tolist() for c in self.classes}

    @abstractmethod
    def __getitem__(self, idx: int) -> Tuple[Any, int]:
        return NotImplemented


class ClassificationDatasetSubset(ClassificationDataset[T]):
    """Subset for ClassificationDataset that behaves like a ClassificationDataset."""

    def __init__(self, dataset: ClassificationDataset[T], indices: List[int]) -> None:
        super().__init__([dataset.reverse_class_encoding[t] for t in np.array(dataset.targets)[indices].tolist()])
        self.dataset = dataset
        self.indices = indices

    def __len__(self) -> int:
        return len(self.indices)

    def __getitem__(self, index: int) -> Tuple[Any, int]:
        x, y = self.dataset[self.indices[index]]
        return x, self.class_encoding[self.dataset.reverse_class_encoding[y]]


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

        super().__init__([i[1] for i in self.items])
        self.base_dir = base_dir
        self.duration = duration

    def __len__(self) -> int:
        return len(self.items)

    def __getitem__(self, idx: int) -> Tuple[Tuple[int, np.ndarray], int]:
        samples, sample_rate = librosa.load(self.base_dir / self.AUDIO_DIRNAME / self.items[idx][0], sr=None)
        samples = np.resize(samples, (self.duration * sample_rate,))
        return (sample_rate, samples), self.targets[idx]


class FSD50KSpectro(FSD50K):
    """Dataset of FSD50K sound data as spectrograms.

    Item:
        x: (?, ?, ?) - spectrogram array
        y: str - target class
    """

    def __init__(self, base_dir: Union[str, Path], duration: float = 5,
                 n_fft: int = 2048, hop_length: int = 1024) -> None:
        super().__init__(base_dir, duration)
        self.n_fft = n_fft
        self.hop_length = hop_length

    def __getitem__(self, idx: int) -> Tuple[np.ndarray, int]:
        (rate, samples), y = super().__getitem__(idx)
        spect = librosa.feature.melspectrogram(y=samples, sr=rate, n_fft=self.n_fft, hop_length=self.hop_length)
        spect = librosa.power_to_db(spect, ref=np.max)
        spect = librosa.util.normalize(spect)
        return spect[np.newaxis, ...], y


class MinimumSamplesDataset(ClassificationDatasetSubset[T]):
    """Dataset wrapper to drop classes that have less than specified number of samples per each class."""

    def __init__(self, dataset: ClassificationDataset[T], n_samples: int) -> None:
        super().__init__(dataset, [i for ids in dataset.class_to_ids.values() if len(ids) >= n_samples for i in ids])


class StandardSplit(LightningDataModule):
    """DataModule with data divided into train/val/test sets according to indices."""

    def __init__(self,
                 dataset: Dataset,
                 train_ids: List[int],
                 val_ids: List[int],
                 test_ids: List[int],
                 batch_size: int = 1,
                 shuffle: bool = True) -> None:
        super().__init__()
        self.dataset = dataset
        self.train_ids = train_ids
        self.val_ids = val_ids
        self.test_ids = test_ids
        self.batch_size = batch_size
        self.shuffle = shuffle

    def setup(self, stage: Optional[str] = None) -> None:
        self.train_set = Subset(self.dataset, self.train_ids)
        self.val_set = Subset(self.dataset, self.val_ids)
        self.test_set = Subset(self.dataset, self.test_ids)

    def train_dataloader(self) -> DataLoader:
        return DataLoader(self.train_set, batch_size=self.batch_size, shuffle=self.shuffle)

    def val_dataloader(self) -> DataLoader:
        return DataLoader(self.val_set, batch_size=self.batch_size, shuffle=self.shuffle)

    def test_dataloader(self) -> DataLoader:
        return DataLoader(self.test_set, batch_size=self.batch_size, shuffle=self.shuffle)


class RandomSplit(StandardSplit):
    """DataModule with data randomly divided into train/val/test sets."""

    def __init__(self, dataset: SizedDataset, batch_size: int = 1, shuffle: bool = True,
                 test_ratio: float = 0.15, val_ratio: float = 0.05) -> None:
        train_ids, val_ids, test_ids = self._get_indices(len(dataset), test_ratio, val_ratio)
        super().__init__(dataset, train_ids, val_ids, test_ids, batch_size, shuffle)

    @staticmethod
    def _get_indices(length: int, test_ratio: float, val_ratio: float) -> Tuple[List[int], List[int], List[int]]:
        cdf = np.cumsum([1 - test_ratio - val_ratio, val_ratio, test_ratio])[:-1]
        stops = (cdf * length).round().astype(int)
        return tuple(x.tolist() for x in np.split(np.random.permutation(length), stops))


class StandardSplitTrainingLimited(StandardSplit):
    """StandardSplit with limited number of examples per class in the training set."""

    def __init__(self, dataset: ClassificationDataset[T], k: int,
                 batch_size: int = 1, shuffle: bool = True, val_ratio: float = 0.05) -> None:
        train_ids, val_ids, test_ids = self._get_indices(dataset.targets, k, val_ratio)
        super().__init__(dataset, train_ids, val_ids, test_ids, batch_size, shuffle)

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


class FewShotTasks(SizedIterableDataset):
    """Dataset wrapper for sampling few-shot learning tasks.

    Item:
        support: (n_classes, n_support, *) - support set
        query: (n_query, *) - query set
        y: (n_query) - true classes of query set
    """

    def __init__(self, base_dataset: ClassificationDataset, n_classes: int, n_support: int, n_query: int,
                 n_tasks: Optional[int] = None) -> None:
        super().__init__()
        self.base_dataset = MinimumSamplesDataset(base_dataset, n_support + 1)
        self.n_classes = n_classes
        self.n_support = n_support
        self.n_query = n_query
        self.n_tasks = n_tasks or len(self.base_dataset.classes) // n_classes

    def __len__(self) -> int:
        return self.n_tasks

    def __iter__(self) -> Iterator[Tuple[np.ndarray, np.ndarray, np.ndarray]]:
        for _ in range(self.n_tasks):
            picked_classes = random.sample(self.base_dataset.classes, self.n_classes)
            cti = self.base_dataset.class_to_ids
            support_ids = [random.sample(cti[c], self.n_support) for c in picked_classes]
            query_id_pool = [i for c in picked_classes for i in cti[c]
                             if i not in {j for ids in support_ids for j in ids}]
            query_ids = random.sample(query_id_pool, self.n_query)
            support = np.array([[self.base_dataset[i][0] for i in ids] for ids in support_ids])
            query = np.array([self.base_dataset[i][0] for i in query_ids])
            y = np.array([picked_classes.index(self.base_dataset.targets[i]) for i in query_ids])
            yield support, query, y


class FewShotSplit(LightningDataModule):
    """DataModule with FewShotTasks divided into train/val/test."""

    def __init__(self,
                 dataset: ClassificationDataset,
                 n_classes: int,
                 n_support: int,
                 n_query: int,
                 n_tasks: int,
                 test_ratio: float = 0.15,
                 val_ratio: float = 0.05,
                 batch_size: int = 1) -> None:
        super().__init__()
        self.dataset = dataset
        self.n_classes = n_classes
        self.n_support = n_support
        self.n_query = n_query
        self.n_tasks = n_tasks
        self.test_ratio = test_ratio
        self.val_ratio = val_ratio
        self.batch_size = batch_size

    def setup(self, stage: Optional[str] = None) -> None:
        base, novel = class_split(self.dataset, self.test_ratio)
        train, val = class_split(base, self.val_ratio / (1 - self.test_ratio))
        self.train = FewShotTasks(train, self.n_classes, self.n_support, self.n_query,
                                  int(self.n_tasks * (1 - self.val_ratio - self.test_ratio)))
        self.val = FewShotTasks(val, self.n_classes, self.n_support, self.n_query,
                                int(self.n_tasks * self.val_ratio))
        self.test = FewShotTasks(novel, self.n_classes, self.n_support, self.n_query,
                                 int(self.n_tasks * self.test_ratio))

    def train_dataloader(self) -> DataLoader:
        return DataLoader(self.train, batch_size=self.batch_size)

    def val_dataloader(self) -> DataLoader:
        return DataLoader(self.val, batch_size=self.batch_size)

    def test_dataloader(self) -> DataLoader:
        return DataLoader(self.test, batch_size=self.batch_size)
