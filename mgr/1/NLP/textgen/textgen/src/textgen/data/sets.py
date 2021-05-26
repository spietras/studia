import random
from typing import Iterator, List, Tuple

import numpy as np
from textgen.data.utils import StreamGenerator, Tokenizer, TreeBankWordTokenizer, Corpus, SentenceCompletionConfig
from textgen.data.utils import StringStreamGenerator, PunktSentenceTokenizer
from torch.utils.data import IterableDataset


class Sentences(IterableDataset):
    """Iterable dataset of sentences.

    Usage:
        >>> stream_generator = StringStreamGenerator("First sentence. Second sentence.")
        >>> sentence_tokenizer = PunktSentenceTokenizer()
        >>> sentences = Sentences(stream_generator, sentence_tokenizer)
        >>> print(next(iter(sentences)))
        'First sentence.'
    """

    def __init__(self, stream_generator: StreamGenerator,
                 sentence_tokenizer: Tokenizer) -> None:
        super().__init__()
        self.stream_generator = stream_generator
        self.sentence_tokenizer = sentence_tokenizer

    def __iter__(self) -> Iterator[str]:
        for stream in self.stream_generator.generate():
            for sentence in self.sentence_tokenizer.tokenize(stream.read()):
                yield sentence


class TokensInSentences(IterableDataset):
    """Iterable dataset of tokens in sentences.

    Usage:
        >>> stream_generator = StringStreamGenerator("First sentence. Second sentence.")
        >>> sentence_tokenizer = PunktSentenceTokenizer()
        >>> sentences = Sentences(stream_generator, sentence_tokenizer)
        >>> word_tokenizer = TreeBankWordTokenizer()
        >>> tokens = TokensInSentences(sentences, word_tokenizer)
        >>> print(next(iter(tokens)))
        ['First', 'sentence', '.']
    """

    def __init__(self, sentences: Sentences, word_tokenizer: Tokenizer) -> None:
        super().__init__()
        self.sentences = sentences
        self.word_tokenizer = word_tokenizer

    def __iter__(self) -> Iterator[List[str]]:
        for sentence in self.sentences:
            yield list(self.word_tokenizer.tokenize(sentence))


class IndicesInSentences(IterableDataset):
    """Wrapper for TokensInSentences dataset to return indices instead of tokens."""

    def __init__(self, tokens: TokensInSentences, corpus: Corpus) -> None:
        super().__init__()
        self.tokens = tokens
        self.corpus = corpus

    def __iter__(self) -> Iterator[List[int]]:
        for tokens in self.tokens:
            yield [self.corpus.tokens_to_indices[token] for token in tokens]


class SentenceCompletionDataset(IterableDataset):
    """Wrapper for TokensInSentences dataset to return random division of sentences as numpy arrays.

    Returns tuple of source sequence, target sequence, source sequence mask of content,
    target sequence mask of content and true output target sequence.
    """

    def __init__(self, tokens: TokensInSentences, config: SentenceCompletionConfig) -> None:
        super().__init__()
        self.config = config
        self.indices = IndicesInSentences(tokens, self.config.corpus)

    def __iter__(self) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        for sentence in self.indices:
            split_point = random.randint(0, len(sentence))
            start = self.config.padder.pad_with_index([self.config.corpus.start_id] + sentence[:split_point])
            end = self.config.padder.pad_with_index([self.config.corpus.start_id] + sentence[split_point:] +
                                                    [self.config.corpus.end_id])
            y = end[1:] + [self.config.corpus.pad_id]
            start, end, y = (np.asarray(start)[:self.config.max_length],
                             np.asarray(end)[:self.config.max_length],
                             np.asarray(y)[:self.config.max_length])
            yield start, end, start != self.config.corpus.pad_id, end != self.config.corpus.pad_id, y
