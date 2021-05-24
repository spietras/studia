import random
from typing import Iterator, List, TextIO, Optional, Tuple, Callable

import numpy as np
from ordered_set import OrderedSet
from textgen.data.utils import Corpus, StreamGenerator, Tokenizer, Counter
from textgen.data.utils import StringStreamGenerator, PunktSentenceTokenizer, TreeBankWordTokenizer
from torch.utils.data import IterableDataset


class Sentences(Corpus, IterableDataset):
    """Iterable dataset of sentences.

    Usage:
        >>> stream_generator = StringStreamGenerator("First sentence. Second sentence.")
        >>> sentence_tokenizer = PunktSentenceTokenizer()
        >>> word_tokenizer = TreeBankWordTokenizer()
        >>> sentences = Sentences(stream_generator, sentence_tokenizer, word_tokenizer)
        >>> print(next(iter(sentences)))
        ['First', 'sentence', '.']
    """

    def __init__(self, stream_generator: StreamGenerator,
                 sentence_tokenizer: Tokenizer, word_tokenizer: Tokenizer,
                 vocabulary: Optional[OrderedSet[str]] = None,
                 n_sentences: Optional[int] = None,
                 eval_hooks: Optional[List[Callable[[List[str]], None]]] = None) -> None:
        v = vocabulary or OrderedSet()
        ns = Counter(n_sentences or 0)
        if eval_hooks is not None or vocabulary is None or n_sentences is None:
            eval_hooks = eval_hooks or []
            self._evaluate(stream_generator.generate(), sentence_tokenizer, word_tokenizer,
                           eval_hooks + [(lambda _: None) if vocabulary else (lambda t: v.update(t)),
                                         (lambda _: None) if n_sentences else (lambda _: ns.inc())])
        super().__init__(v)
        self.stream_generator = stream_generator
        self.sentence_tokenizer = sentence_tokenizer
        self.word_tokenizer = word_tokenizer
        self.n_sentences = ns.count

    @staticmethod
    def _evaluate(streams: Iterator[TextIO],
                  sentence_tokenizer: Tokenizer, word_tokenizer: Tokenizer,
                  eval_hooks: List[Callable[[List[str]], None]]) -> None:
        for stream in streams:
            for sentence in sentence_tokenizer.tokenize(stream.read()):
                tokens = list(word_tokenizer.tokenize(sentence))
                for hook in eval_hooks:
                    hook(tokens)

    def __len__(self) -> int:
        return self.n_sentences

    def __iter__(self) -> Iterator[List[str]]:
        for stream in self.stream_generator.generate():
            for sentence in self.sentence_tokenizer.tokenize(stream.read()):
                yield list(self.word_tokenizer.tokenize(sentence))


class SentencesWithIndices(IterableDataset):
    """Wrapper for Sentence dataset to return indices instead of tokens."""

    def __init__(self, sentences: Sentences) -> None:
        super().__init__()
        self.sentences = sentences

    def __iter__(self) -> Iterator[List[int]]:
        for sentence in self.sentences:
            yield [self.sentences.tokens_to_indices[token] for token in sentence]


class SentenceCompletionDataset(IterableDataset):
    """Wrapper for Sentence dataset to return random division of sentences as numpy arrays.

    Returns tuple of source sequence, target sequence, source sequence mask of content,
    target sequence mask of content and true output target sequence.
    """

    def __init__(self, sentences: Sentences,
                 max_length: int,
                 pad_token: str = "<P>", start_token: str = "<S>", end_token: str = "<E>") -> None:
        super().__init__()
        self.max_length = max_length
        self.pad_token = pad_token
        self.start_token = start_token
        self.end_token = end_token
        sentences = Sentences(sentences.stream_generator, sentences.sentence_tokenizer, sentences.word_tokenizer,
                              sentences.tokens, sentences.n_sentences)
        sentences.add_extra_tokens([pad_token, start_token, end_token])
        self.pad_id = sentences.tokens_to_indices[pad_token]
        self.start_id = sentences.tokens_to_indices[start_token]
        self.end_id = sentences.tokens_to_indices[end_token]
        self.sentences_width_indices = SentencesWithIndices(sentences)
        self.sentences = self.sentences_width_indices.sentences

    def pad(self, lst: List[int]) -> List[int]:
        return lst + [self.pad_id] * max(0, (self.max_length - len(lst)))

    def __iter__(self) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        for sentence in self.sentences_width_indices:
            split_point = random.randint(1, len(sentence) - 1)
            start = self.pad([self.start_id] + sentence[:split_point])
            end = self.pad([self.start_id] + sentence[split_point:] + [self.end_id])
            y = end[1:] + [self.pad_id]
            start, end, y = (np.asarray(start)[:self.max_length],
                             np.asarray(end)[:self.max_length],
                             np.asarray(y)[:self.max_length])
            yield start, end, start != self.pad_id, end != self.pad_id, y
