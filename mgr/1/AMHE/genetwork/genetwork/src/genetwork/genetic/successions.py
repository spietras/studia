from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List

from genetwork.genetic.creatures import Creature, Gene
from genetwork.genetic.evaluators import Evaluator

T = TypeVar('T', bound=Gene)


class Succession(ABC, Generic[T]):
    @abstractmethod
    def pick(self, parents: List[Creature[T]], offspring: List[Creature[T]]) -> List[Creature[T]]:
        return NotImplemented


class BestOverallSuccession(Succession[T]):
    def __init__(self, evaluator: Evaluator) -> None:
        self.evaluator = evaluator

    def pick(self, parents: List[Creature[T]], offspring: List[Creature[T]]) -> List[Creature[T]]:
        population = parents + offspring
        population.sort(key=lambda x: self.evaluator.evaluate_creature(x))
        new_population = population[:len(parents)]
        return new_population
