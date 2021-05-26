import itertools
from abc import ABC, abstractmethod
from typing import List

from textgen.data.utils import SentenceCompletionConfig
from textgen.model.transformer import Transformer
from torch import tensor


class SentenceGenerator(ABC):
    """Base class for sentence generators."""

    @abstractmethod
    def generate(self, prompt: str) -> str:
        return NotImplemented


class SentenceIndicesGenerator(SentenceGenerator):
    """Sentence generator that internally uses indices, instead of tokens."""

    def __init__(self, config: SentenceCompletionConfig) -> None:
        super().__init__()
        self.config = config

    def generate(self, prompt: str) -> str:
        prompt_tokens = list(self.config.word_tokenizer.tokenize(prompt))
        try:
            prompt_indices = list(self.config.corpus.convert_to_indices(prompt_tokens))
        except KeyError as e:
            raise ValueError(f"Invalid token: {e}")
        new_indices = list(self.generate_indices(prompt_indices))
        new_tokens = list(self.config.corpus.convert_to_token(new_indices))
        return self.config.word_tokenizer.detokenize(prompt_tokens + new_tokens)

    @abstractmethod
    def generate_indices(self, indices: List[int]) -> List[int]:
        return NotImplemented


class TransformerSentenceIndicesGenerator(SentenceIndicesGenerator):
    """SentenceIndicesGenerator that uses TransformerGenerator to generate indices."""

    def __init__(self, model: Transformer) -> None:
        super().__init__(model.config)
        self.model = model

    def generate_indices(self, indices: List[int]) -> List[int]:
        src = self.model.config.padder.pad_with_index(indices)
        src = tensor([src], device=self.model.device)
        _, out = self.model.generate(src)
        return list(itertools.takewhile(lambda x: x != self.model.config.corpus.end_id, out[0].tolist()))
