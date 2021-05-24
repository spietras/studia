from abc import ABC, abstractmethod
from io import StringIO
from pathlib import Path
from typing import Iterator, TextIO, Dict

import nltk
from bidict import bidict
from nltk.tokenize.treebank import TreebankWordDetokenizer, TreebankWordTokenizer
from ordered_set import OrderedSet


class Counter:
    """Counter utility for mutable counting."""

    def __init__(self, count: int = 0) -> None:
        super().__init__()
        self.count = count

    def inc(self) -> 'Counter':
        self.count += 1
        return self

    def dec(self) -> 'Counter':
        self.count -= 1
        return self


class Tokenizer(ABC):
    """Two-way tokenizer base class."""

    @abstractmethod
    def tokenize(self, text: str) -> Iterator[str]:
        """Turn string into tokens."""
        return NotImplemented

    @abstractmethod
    def detokenize(self, tokens: Iterator[str]) -> str:
        """Turn tokens into string."""
        return NotImplemented


class TreeBankWordTokenizer(Tokenizer):
    """Word-level tokenizer using nltk's Treebank."""

    def __init__(self) -> None:
        super().__init__()
        self.tokenizer = TreebankWordTokenizer()
        self.detokenizer = TreebankWordDetokenizer()

    def tokenize(self, text: str) -> Iterator[str]:
        return iter(self.tokenizer.tokenize(text))

    def detokenize(self, tokens: Iterator[str]) -> str:
        return self.detokenizer.detokenize(tokens)


class PunktSentenceTokenizer(Tokenizer):
    """Sentence-level tokenizer using nltk's Punkt.

    Since sentences are easy to connect, we are simple detokenizing by joining on specified separator.
    """

    def __init__(self, separator: str = " ") -> None:
        super().__init__()
        nltk.download('punkt', quiet=True)
        self.punkt = nltk.data.load('tokenizers/punkt/english.pickle')
        self.separator = separator

    def tokenize(self, text: str) -> Iterator[str]:
        return iter(self.punkt.tokenize(text))

    def detokenize(self, tokens: Iterator[str]) -> str:
        return self.separator.join(tokens)


class StreamGenerator(ABC):
    """Stream generator base class."""

    @abstractmethod
    def generate(self) -> Iterator[TextIO]:
        return NotImplemented


class StringStreamGenerator(StreamGenerator):
    """Generate stream from single string."""

    def __init__(self, text: str) -> None:
        super().__init__()
        self.text = text

    def generate(self) -> Iterator[TextIO]:
        yield StringIO(self.text)


class FilesStreamGenerator(StreamGenerator):
    """Generate streams from files."""

    def __init__(self, files: Iterator[Path]) -> None:
        super().__init__()
        self.files = files

    def generate(self) -> Iterator[TextIO]:
        for file in self.files:
            yield open(file)


class FilesInDirectoryStreamGenerator(FilesStreamGenerator):
    """Generate streams from files in a directory using glob pattern."""

    def __init__(self, dir: Path, glob: str = '*') -> None:
        super().__init__(dir.expanduser().resolve().glob(glob))


class Corpus:
    """Corpus base class."""

    _tokens: OrderedSet[str] = OrderedSet()

    def __init__(self, tokens: OrderedSet[str]) -> None:
        super().__init__()
        self._tokens = tokens

    def add_extra_token(self, token: str) -> None:
        self._tokens.add(token)

    def add_extra_tokens(self, tokens: Iterator[str]) -> None:
        for token in tokens:
            self.add_extra_token(token)

    def change_tokens(self, new: OrderedSet[str]) -> None:
        self._tokens = new

    @property
    def tokens(self) -> OrderedSet[str]:
        """All tokens available in a corpus. """
        return self._tokens

    @property
    def vocabulary(self) -> bidict[int, str]:
        """Bidirectional mapping from indices to tokens (and vice versa)."""
        return bidict({i: word for i, word in enumerate(self.tokens)})

    @property
    def indices_to_tokens(self) -> Dict[int, str]:
        """Mapping from indices to tokens."""
        return dict(self.vocabulary)

    @property
    def tokens_to_indices(self) -> Dict[str, int]:
        """Mapping from tokens to indices."""
        return dict(self.vocabulary.inverse)

    def convert_to_token(self, indices: Iterator[int]) -> Iterator[str]:
        """Converts indices to tokens."""
        itt = self.indices_to_tokens
        return (itt[i] for i in indices)

    def convert_to_indices(self, tokens: Iterator[str]) -> Iterator[int]:
        """Converts tokens to indices."""
        tti = self.tokens_to_indices
        return (tti[token] for token in tokens)
