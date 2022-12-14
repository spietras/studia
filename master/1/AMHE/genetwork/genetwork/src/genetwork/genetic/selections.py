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
    def __init__(self, evaluator: Evaluator[T], threshold: float, size_factor: float):
        self.evaluator = evaluator
        self.threshold = threshold
        self.size_factor = size_factor

    def select(self, population: List[Creature[T]]) -> List[Creature[T]]:
        population = sorted(population, key=lambda x: self.evaluator.evaluate_creature(x))
        new_pop_idx = random.choices(range(0, int(self.threshold * len(population))),
                                     k=int(self.size_factor * len(population)))
        return [population[idx] for idx in new_pop_idx]
