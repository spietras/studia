from abc import ABC, abstractmethod
from numbers import Real
from typing import TypeVar, Generic

from genetwork.genetic.creatures import Creature, SndlibGene, Gene

T = TypeVar('T', bound=Gene)


class Evaluator(ABC, Generic[T]):
    @abstractmethod
    def evaluate(self, a: Creature[T]) -> Real:
        return NotImplemented


class SndlibEvaluator(Evaluator[SndlibGene]):
    @abstractmethod
    def evaluate(self, a: Creature[SndlibGene]) -> Real:
        return NotImplemented


class TotalCostWithConstraintsSndlibEvaluator(SndlibEvaluator):
    def evaluate(self, a: Creature[SndlibGene]) -> Real:
        pass  # TODO
