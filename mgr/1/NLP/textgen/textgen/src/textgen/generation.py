import random
from abc import ABC, abstractmethod
from typing import List, Tuple

from textgen.data.utils import SentenceCompletionConfig
from textgen.model.transformer import TransformerModel
from torch import Tensor


class TransformerGenerator(ABC):
    """Base class for transformer sequence generators."""

    def __init__(self, model: TransformerModel) -> None:
        super().__init__()
        self.model = model

    def generate(self, src: Tensor, src_mask: Tensor) -> Tuple[Tensor, Tensor]:
        pass  # TODO
        # src = self.sentences.pad([self.sentences.start_id] + indices)
        # src = Tensor([src]).long()
        # out_indices = []
        # for i in range(self.sentences.max_length):
        #     trg = Tensor(self.sentences.pad([self.sentences.start_id] + out_indices)).long()
        #     out = self.model(src, trg, src != self.sentences.pad_id, trg != self.sentences.pad_id)
        #     out = self.choose_token(out[0][i])
        #     if out == self.sentences.end_id:
        #         break
        #     out_indices.append(out)
        # return out_indices

    @abstractmethod
    def choose_index(self, predictions: Tensor) -> int:
        return NotImplemented


class TransformerGreedyGenerator(TransformerGenerator):
    """TransformerGenerator that always picks the most probable index."""

    def choose_index(self, predictions: Tensor) -> int:
        return predictions.argmax().long().item()


class TransformerProbabilisticGenerator(TransformerGenerator):
    """TransformerGenerator that picks index based on probability distribution."""

    def choose_index(self, predictions: Tensor) -> int:
        return random.choices(range(len(predictions)), predictions.exp())[0]


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
        prompt_tokens_it = self.config.word_tokenizer.tokenize(prompt)
        prompt_tokens = list(prompt_tokens_it)
        prompt_indices = list(self.config.corpus.convert_to_indices(prompt_tokens_it))
        new_indices = self.generate_indices(prompt_indices)
        new_tokens = list(self.config.corpus.convert_to_token(new_indices))
        return self.config.word_tokenizer.detokenize(prompt_tokens + new_tokens)

    @abstractmethod
    def generate_indices(self, indices: List[int]) -> List[int]:
        return NotImplemented


class TransformerSentenceIndicesGenerator(SentenceIndicesGenerator):
    """SentenceIndicesGenerator that uses TransformerGenerator to generate indices."""

    def __init__(self, config: SentenceCompletionConfig, generator: TransformerGenerator) -> None:
        super().__init__(config)
        self.generator = generator

    def generate_indices(self, indices: List[int]) -> List[int]:
        src = self.config.padder.pad_with_index(indices)[:self.config.max_length]
        src = Tensor([src]).long()
        _, out = self.generator.generate(src, src != self.config.corpus.pad_id)
        return out.tolist()
