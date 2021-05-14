import copy
import random
from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List

from genetwork.genetic.creatures import Creature, Gene
from genetwork.genetic.evaluators import Evaluator

T = TypeVar('T', bound=Gene)


class Selection(ABC, Generic[T]):
    @abstractmethod
    def select(self, population: List[Creature[T]]) -> List[Creature[T]]:
        return NotImplemented


class ThresholdSelection(Selection[T]):
    def __init__(self, evaluator: Evaluator, threshold: int, size: int):
        self.evaluator = evaluator
        self.threshold = threshold
        self.size = size

    def select(self, population: List[Creature[T]]) -> List[Creature[T]]:
        population.sort(key=lambda x: self.evaluator.evaluate_creature(x))
        new_pop_idx = random.choices(range(0, self.threshold), k=self.size)
        new_pop = [copy.deepcopy(population[idx]) for idx in new_pop_idx]
        return new_pop
