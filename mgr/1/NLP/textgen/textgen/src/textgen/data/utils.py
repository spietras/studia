from abc import ABC, abstractmethod
from io import StringIO
from pathlib import Path
from typing import Iterator, TextIO, Dict, Any, Callable, List

import nltk
import truecase
from nltk.tokenize.treebank import TreebankWordDetokenizer, TreebankWordTokenizer
from ordered_set import OrderedSet


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


class WordTokenizer(Tokenizer):

    def tokenize(self, text: str) -> Iterator[str]:
        for token in self.tokenize_normalized(text):
            yield token.lower()

    @abstractmethod
    def tokenize_normalized(self, text: str) -> Iterator[str]:
        return NotImplemented

    def detokenize(self, tokens: Iterator[str]) -> str:
        return truecase.get_true_case(self.detokenize_normalized(tokens))

    @abstractmethod
    def detokenize_normalized(self, tokens: Iterator[str]) -> str:
        return NotImplemented


class TreeBankWordTokenizer(WordTokenizer):
    """Word-level tokenizer using nltk's Treebank."""

    def __init__(self) -> None:
        super().__init__()
        self.tokenizer = TreebankWordTokenizer()
        self.detokenizer = TreebankWordDetokenizer()

    def tokenize_normalized(self, text: str) -> Iterator[str]:
        return iter(self.tokenizer.tokenize(text))

    def detokenize_normalized(self, tokens: Iterator[str]) -> str:
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

    def __init__(self, files: List[Path]) -> None:
        super().__init__()
        self.files = files

    def generate(self) -> List[TextIO]:
        for file in self.files:
            yield open(file)


class FilesInDirectoryStreamGenerator(FilesStreamGenerator):
    """Generate streams from files in a directory using glob pattern."""

    def __init__(self, dir: Path, glob: str = '*') -> None:
        super().__init__(list(dir.expanduser().resolve().glob(glob)))


class Corpus:
    """Corpus base class."""

    _vocabulary: OrderedSet[str] = OrderedSet()

    def __init__(self, vocabulary: OrderedSet[str]) -> None:
        super().__init__()
        self._vocabulary = vocabulary

    def add_extra_token(self, token: str) -> None:
        self._vocabulary.add(token)

    def add_extra_tokens(self, tokens: Iterator[str]) -> None:
        for token in tokens:
            self.add_extra_token(token)

    def change_tokens(self, new: OrderedSet[str]) -> None:
        self._vocabulary = new

    @property
    def vocabulary(self) -> OrderedSet[str]:
        """All tokens available in a corpus. """
        return self._vocabulary

    @property
    def indices_to_tokens(self) -> Dict[int, str]:
        """Mapping from indices to tokens."""
        return {i: word for i, word in enumerate(self.vocabulary)}

    @property
    def tokens_to_indices(self) -> Dict[str, int]:
        """Mapping from tokens to indices."""
        return {word: i for i, word in enumerate(self.vocabulary)}

    def convert_to_token(self, indices: Iterator[int]) -> Iterator[str]:
        """Converts indices to tokens."""
        itt = self.indices_to_tokens
        return (itt[i] for i in indices)

    def convert_to_indices(self, tokens: Iterator[str]) -> Iterator[int]:
        """Converts tokens to indices."""
        tti = self.tokens_to_indices
        return (tti[token] for token in tokens)


class CorpusEvaluator:
    """Create Corpus from streams with additional hooks."""

    def __init__(self, sentence_tokenizer: Tokenizer, word_tokenizer: Tokenizer) -> None:
        super().__init__()
        self.sentence_tokenizer = sentence_tokenizer
        self.word_tokenizer = word_tokenizer
        self.sentence_hooks = []
        self.word_hooks = []

    def on_sentence(self, hook: Callable[[str], Any]) -> None:
        self.sentence_hooks.append(hook)

    def on_word(self, hook: Callable[[str], Any]) -> None:
        self.word_hooks.append(hook)

    def evaluate(self, streams: Iterator[TextIO]) -> Corpus:
        vocabulary = OrderedSet()
        for stream in streams:
            for sentence in self.sentence_tokenizer.tokenize(stream.read()):
                for hook in self.sentence_hooks:
                    hook(sentence)
                for word in self.word_tokenizer.tokenize(sentence):
                    vocabulary.append(word)
                    for hook in self.word_hooks:
                        hook(word)
        return Corpus(vocabulary)


class CompletionCorpus(Corpus):
    """Corpus with added pad, start and end tokens."""

    def __init__(self, vocabulary: OrderedSet[str],
                 pad_token: str = "<P>", start_token: str = "<S>", end_token: str = "<E>") -> None:
        super().__init__(OrderedSet([pad_token, start_token, end_token]) | vocabulary)
        self.pad_token = pad_token
        self.start_token = start_token
        self.end_token = end_token
        self.pad_id = self.tokens_to_indices[pad_token]
        self.start_id = self.tokens_to_indices[start_token]
        self.end_id = self.tokens_to_indices[end_token]


class SentencePadder:
    """Utility class for padding sentences."""

    def __init__(self, max_length: int, corpus: CompletionCorpus) -> None:
        super().__init__()
        self.max_length = max_length
        self.corpus = corpus

    def pad_with_token(self, sentence: List[str]) -> List[str]:
        return sentence + [self.corpus.pad_token] * max(0, (self.max_length - len(sentence)))

    def pad_with_index(self, sentence: List[int]) -> List[int]:
        return sentence + [self.corpus.pad_id] * max(0, (self.max_length - len(sentence)))


class SentenceCompletionConfig:
    """Configuration holder for sentence completion."""

    def __init__(self, corpus: CompletionCorpus,
                 sentence_tokenizer: Tokenizer,
                 word_tokenizer: Tokenizer,
                 max_length: int) -> None:
        super().__init__()
        self.corpus = corpus
        self.sentence_tokenizer = sentence_tokenizer
        self.word_tokenizer = word_tokenizer
        self.max_length = max_length
        self.padder = SentencePadder(max_length, corpus)
