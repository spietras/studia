"""Module for code related to data."""
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional, Tuple, List, Set, Dict, TypeVar, Hashable, Generic, Iterator

from scipy import signal
from scipy.io import wavfile
from os import listdir
from os.path import isfile, join
import os
import pandas as pd


import numpy as np
from pytorch_lightning import LightningDataModule
from torch.utils.data import DataLoader, Dataset, IterableDataset, random_split, Subset
from torch.random import initial_seed, manual_seed
from torch import Generator


T = TypeVar('T', bound=Hashable)


class ClassificationDataset(Dataset, ABC, Generic[T]):
    @property
    def classes(self) -> Set[T]:
        return set(self.class_to_ids.keys())

    @property
    @abstractmethod
    def targets(self) -> List[T]:
        return NotImplemented

    @property
    def class_to_ids(self) -> Dict[T, List[int]]:
        target_array = np.array(self.targets)
        return {c: np.where(target_array == c)[0].tolist() for c in self.targets}


class FSDK50K(ClassificationDataset[str]):
    """Dataset of FSD50K sound data.

    Item:
        x:
            rate: int - sample rate
            data: (n_samples, n_channels) - sample values for each audio channel
        y: str - target class
    """
   

    def __init__(self, base_dir: Path, white_list: Path) -> None:
        super().__init__()
        self.items:List[Tuple[str,str]]=[]
        onlycsvfiles = [f for f in listdir(base_dir) if isfile(base_dir.joinpath(f)) and f.endswith(".csv")]
        white_list= pd.read_csv(str(white_list), usecols=[0], names=['heading'])
        white_list_set=set(white_list['heading'].unique())
        description = pd.read_csv(base_dir.joinpath(onlycsvfiles[0]),sep=",",quotechar="\"")	
        index_control=[] # for checking rejected samples
        for i in range(description.shape[0]-1):
         
         fname=str(description.iloc[i,0])
         
         if (str(description.iloc[i,1])=="nan"): # situation when description = [first,"second,third,fourth",last] -read csv does not work well = whole in deccription[i,0]
          fname_parse_list=fname.split("\"")
          fname=str(fname_parse_list[0][:-1])
          label=str(fname_parse_list[1])  
         else: label=str(description.iloc[i,1])
         label_list=label.split(",")
         already_found_index:Int = -1
         duplicate: Boolean = False
         
         for j in range(len(label_list)):
          if (label_list[j] in white_list_set):
           if (already_found_index>=0):
            duplicate=True
            index_control.append((i,label_list[j]))
            break
           else:
            already_found_index=j
         if ((not duplicate) and (already_found_index>=0)): 
          self.items.append((fname,label_list[already_found_index]))
                  
        self.base_dir = base_dir
        

    @property
    def targets(self) -> List[str]:
        pass  # TODO
        test = [u[1] for u in self.items]
        for k in test:
         print(k)
        return test
        

    def __len__(self) -> int:
        return len(self.items)

    def __getitem__(self, idx: int) -> Tuple[Tuple[int, np.ndarray], str]:
        sample_rate, samples = wavfile.read(self.base_dir.joinpath(self.items[idx][0]+".wav"))
        if (samples.ndim==1): samples = np.resize(samples,(5*sample_rate,))
        else: samples = np.resize(samples,(5*sample_rate,samples.shape[1]))
        return ((sample_rate,samples),self.items[idx][1])

class FSDK50KSpectro(FSDK50K):
    """Dataset of FSD50K sound data as spectrograms.

    Item:
        x: (?, ?, ?) - spectrogram array
        y: str - target class
    """

    def __getitem__(self, idx: int) -> Tuple[np.ndarray, str]:
        (rate, samples), y = super().__getitem__(idx)
        frequencies, times, spectrogram = signal.spectrogram(samples, rate)
        return (spectrogram[np.newaxis,...],y)
       


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
                 batch_size:int) -> None:
        super().__init__()
        self.dataset = dataset
        self.train_ids = train_ids
        self.val_ids = val_ids
        self.test_ids = test_ids
        self.batch_size = batch_size

    def setup(self, stage: Optional[str] = None) -> None:
        # use train_ids, val_ids, test_ids to split
        self.train_set = Subset(self.dataset,self.train_ids)
        self.val_set = Subset(self.dataset,self.val_ids)
        self.test_set = Subset(self.dataset,self.test_ids)

    def train_dataloader(self) -> DataLoader:
        return DataLoader(self.train_set,self.batch_size)
        
    def val_dataloader(self) -> DataLoader:
        return DataLoader(self.val_set,self.batch_size)

    def test_dataloader(self) -> DataLoader:
        return DataLoader(self.test_set,self.batch_size)

class RandomSplit(StandardSplit):
    """DataModule with data randomly divided into train/val/test sets."""

    def __init__(self, dataset: Dataset, batch_size:int, test_ratio: float = 0.15, val_ratio: float = 0.05) -> None:
        train_ids, val_ids, test_ids = self._get_indices(dataset, test_ratio, val_ratio)
        super().__init__(dataset, train_ids, val_ids, test_ids, batch_size)

    @staticmethod
    def _get_indices(dataset: Dataset, test_ratio: float, val_ratio: float) -> Tuple[List[int], List[int], List[int]]:
        return random_split(dataset, [int(dataset.__len__()*(1-test_ratio -val_ratio)),int(dataset.__len__()*val_ratio),int(dataset.__len__()*test_ratio)], generator=Generator())

class StandardSplitTrainingLimited(StandardSplit):
    """StandardSplit with limited number of examples per class in the training set."""

    def __init__(self, dataset: ClassificationDataset, k: int, val_ratio: float = 0.05) -> None:
        train_ids, val_ids, test_ids = self._get_indices(dataset, k, val_ratio)
        super().__init__(dataset, train_ids, val_ids, test_ids)

    @staticmethod
    def _get_indices(dataset: Dataset, k: int, val_ratio: float) -> Tuple[List[int], List[int], List[int]]:
        pass  # TODO


def class_split(dataset: ClassificationDataset,
                novel_ratio: float = 0.2) -> Tuple[ClassificationDataset, ClassificationDataset]:
    """Split dataset into base classes and novel classes."""
    pass  # TODO


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
