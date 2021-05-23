from abc import ABC, abstractmethod
from typing import List

from textgen.data.sets import SentenceCompletionDataset
from textgen.model.transformer import Transformer
from torch import Tensor


class SentenceGenerator(ABC):
    """Base class for sentence generators."""

    @abstractmethod
    def generate(self, prompt: str) -> str:
        return NotImplemented


class SentenceIndicesGenerator(SentenceGenerator):
    """Sentence generator that internally uses indices, instead of tokens."""

    def __init__(self, sentences: SentenceCompletionDataset) -> None:
        super().__init__()
        self.sentences = sentences

    def generate(self, prompt: str) -> str:
        prompt_tokens_it = self.sentences.sentences.word_tokenizer.tokenize(prompt)
        prompt_tokens = list(prompt_tokens_it)
        prompt_indices = list(self.sentences.sentences.convert_to_indices(prompt_tokens_it))
        new_indices = self.generate_indices(prompt_indices)
        new_tokens = list(self.sentences.sentences.convert_to_token(new_indices))
        return self.sentences.sentences.word_tokenizer.detokenize(prompt_tokens + new_tokens)

    @abstractmethod
    def generate_indices(self, indices: List[int]) -> List[int]:
        return NotImplemented


class TransformerSentenceGenerator(SentenceIndicesGenerator):
    """Sentence generator that uses transformer model."""

    def __init__(self, model: Transformer, sentences: SentenceCompletionDataset) -> None:
        super().__init__(sentences)
        self.model = model

    def generate_indices(self, indices: List[int]) -> List[int]:
        src = self.sentences.pad([self.sentences.start_id] + indices)
        src = Tensor([src]).long()
        out_indices = []
        for i in range(self.sentences.max_length):
            trg = Tensor(self.sentences.pad([self.sentences.start_id] + out_indices)).long()
            out = self.model(src, trg, src != self.sentences.pad_id, trg != self.sentences.pad_id)
            out = self.choose_token(out[0][i])
            if out == self.sentences.end_id:
                break
            out_indices.append(out)
        return out_indices

    @abstractmethod
    def choose_token(self, out: Tensor) -> int:
        return NotImplemented


class TransformerGreedySentenceGenerator(TransformerSentenceGenerator):
    """TransformerSentenceGenerator that always picks the most probable index."""

    def choose_token(self, predictions: Tensor) -> int:
        return predictions.argmax().long().item()
