import itertools
from pathlib import Path
from typing import Union

from pytorch_lightning import LightningDataModule
from textgen.data.sets import StreamGenerator, Sentences, SentenceCompletionDataset, TokensInSentences
from textgen.data.utils import CompletionCorpus, SentenceCompletionConfig, PunktSentenceTokenizer, TreeBankWordTokenizer
from textgen.data.utils import FilesInDirectoryStreamGenerator, StringStreamGenerator, Tokenizer, CorpusEvaluator
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
                 sentence_tokenizer: Tokenizer = PunktSentenceTokenizer(),
                 word_tokenizer: Tokenizer = TreeBankWordTokenizer(),
                 batch_size: int = 1,
                 num_workers: int = 0) -> None:
        streams = itertools.chain(train_generator.generate(), val_generator.generate(), test_generator.generate())
        corpus = CorpusEvaluator(sentence_tokenizer, word_tokenizer).evaluate(streams)
        corpus = CompletionCorpus(corpus.vocabulary)
        self.config = SentenceCompletionConfig(corpus, sentence_tokenizer, word_tokenizer, max_length)

        train_tokens = TokensInSentences(Sentences(train_generator, sentence_tokenizer), word_tokenizer)
        val_tokens = TokensInSentences(Sentences(val_generator, sentence_tokenizer), word_tokenizer)
        test_tokens = TokensInSentences(Sentences(test_generator, sentence_tokenizer), word_tokenizer)

        super().__init__(SentenceCompletionDataset(train_tokens, self.config),
                         SentenceCompletionDataset(val_tokens, self.config),
                         SentenceCompletionDataset(test_tokens, self.config),
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
