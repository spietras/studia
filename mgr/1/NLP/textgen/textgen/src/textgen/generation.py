from abc import ABC, abstractmethod
from typing import List

from textgen.model.transformer import TransformerModel, TransformerGenerator
from torch import Tensor


class SentenceGenerator(ABC):
    """Base class for sentence generators."""

    @abstractmethod
    def generate(self, prompt: str, device: str) -> str:
        return NotImplemented


class SentenceIndicesGenerator(SentenceGenerator):
    """Sentence generator that internally uses indices, instead of tokens."""

    def __init__(self, generator: TransformerGenerator) -> None:
        super().__init__()
        self.generator = generator

    def generate(self, prompt: str, device: str) -> str:
        prompt_tokens = list(self.generator.config.word_tokenizer.tokenize(prompt))
        prompt_indices = list(self.generator.config.corpus.convert_to_indices(prompt_tokens))
        new_indices = list(self.generate_indices(prompt_indices, device))
        new_tokens = list(self.generator.config.corpus.convert_to_token(new_indices))
        return self.generator.config.word_tokenizer.detokenize(prompt_tokens + new_tokens)

    @abstractmethod
    def generate_indices(self, indices: List[int], device: str) -> List[int]:
        return NotImplemented


class TransformerSentenceIndicesGenerator(SentenceIndicesGenerator):
    """SentenceIndicesGenerator that uses TransformerGenerator to generate indices."""

    def __init__(self, model: TransformerModel, generator: TransformerGenerator) -> None:
        super().__init__(generator)
        self.model = model
        self.generator = generator

    def generate_indices(self, indices: List[int], device: str) -> List[int]:
        src = self.generator.config.padder.pad_with_index(indices)
        src = Tensor([src]).long()
        _, out = self.generator.generate_sentence(src, self.model, device)
        return out.tolist()
