from pathlib import Path
from typing import Optional, Union

from ordered_set import OrderedSet
from pytorch_lightning import LightningDataModule
from textgen.data.sets import StreamGenerator, Tokenizer, Sentences, SentenceCompletionDataset
from textgen.data.utils import FilesInDirectoryStreamGenerator, StringStreamGenerator
from textgen.data.utils import PunktSentenceTokenizer, TreeBankWordTokenizer
from torch.utils.data import DataLoader, IterableDataset


class IterableSplit(LightningDataModule):
    """DataModule with different iterable datasets for train/val/test."""

    def __init__(self,
                 train_dataset: IterableDataset,
                 val_dataset: IterableDataset,
                 test_dataset: IterableDataset,
                 batch_size: int = 1,
                 num_workers: int = 0) -> None:
        super().__init__()
        self.train_dataset = train_dataset
        self.val_dataset = val_dataset
        self.test_dataset = test_dataset
        self.batch_size = batch_size
        self.num_workers = num_workers

    def train_dataloader(self) -> DataLoader:
        return DataLoader(self.train_dataset, batch_size=self.batch_size, num_workers=self.num_workers)

    def val_dataloader(self) -> DataLoader:
        return DataLoader(self.val_dataset, batch_size=self.batch_size, num_workers=self.num_workers)

    def test_dataloader(self) -> DataLoader:
        return DataLoader(self.test_dataset, batch_size=self.batch_size, num_workers=self.num_workers)


class SentenceCompletionIterableSplit(IterableSplit):
    """DataModule with different train/val/test sentence completion datasets using StreamGenerators."""

    def __init__(self,
                 train_generator: StreamGenerator, val_generator: StreamGenerator, test_generator: StreamGenerator,
                 max_length: int,
                 batch_size: int = 1,
                 num_workers: int = 0,
                 sentence_tokenizer: Tokenizer = PunktSentenceTokenizer(),
                 word_tokenizer: Tokenizer = TreeBankWordTokenizer(),
                 vocabulary: Optional[OrderedSet[str]] = None,
                 train_n_sentences: Optional[int] = None,
                 val_n_sentences: Optional[int] = None,
                 test_n_sentences: Optional[int] = None,
                 pad_token: str = "<P>", start_token: str = "<S>", end_token: str = "<E>") -> None:
        train_sentences = Sentences(train_generator, sentence_tokenizer, word_tokenizer, vocabulary, train_n_sentences)
        val_sentences = Sentences(val_generator, sentence_tokenizer, word_tokenizer, vocabulary, val_n_sentences)
        test_sentences = Sentences(test_generator, sentence_tokenizer, word_tokenizer, vocabulary, test_n_sentences)
        if vocabulary is None:
            vocabulary = train_sentences.tokens | val_sentences.tokens | test_sentences.tokens
            train_sentences.change_tokens(vocabulary)
            val_sentences.change_tokens(vocabulary)
            test_sentences.change_tokens(vocabulary)
        super().__init__(SentenceCompletionDataset(train_sentences, max_length, pad_token, start_token, end_token),
                         SentenceCompletionDataset(val_sentences, max_length, pad_token, start_token, end_token),
                         SentenceCompletionDataset(test_sentences, max_length, pad_token, start_token, end_token),
                         batch_size, num_workers)


class SentenceCompletionIterableSplitFromDir(SentenceCompletionIterableSplit):
    """DataModule with train/val/test split from subdirectories for sentence completion."""

    def __init__(self, root_dir: Union[Path, str],
                 train_subdir: str = "train", val_subdir: str = "val", test_subdir: str = "test",
                 *args, **kwargs) -> None:
        if isinstance(root_dir, str):
            root_dir = Path(root_dir)
        train_dir = root_dir / train_subdir
        val_dir = root_dir / val_subdir
        test_dir = root_dir / test_subdir
        super().__init__(FilesInDirectoryStreamGenerator(train_dir),
                         FilesInDirectoryStreamGenerator(val_dir),
                         FilesInDirectoryStreamGenerator(test_dir),
                         *args, **kwargs)


class SentenceCompletionIterableSplitFromStrings(SentenceCompletionIterableSplit):
    """DataModule with train/val/test split from passed strings for sentence completion."""

    def __init__(self, train_text: str, val_text: str, test_text: str, *args, **kwargs) -> None:
        super().__init__(StringStreamGenerator(train_text),
                         StringStreamGenerator(val_text),
                         StringStreamGenerator(test_text),
                         *args, **kwargs)
