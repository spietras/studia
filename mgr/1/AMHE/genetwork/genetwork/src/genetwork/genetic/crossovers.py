from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List, Tuple

from genetwork.genetic.creatures import Creature, Gene

T = TypeVar('T', bound=Gene)


class Crossover(ABC, Generic[T]):
    @abstractmethod
    def cross(self, population: List[Creature[T]]) -> List[Creature[T]]:
        return NotImplemented

    @abstractmethod
    def cross_two(self, a: Creature[T], b: Creature[T]) -> Tuple[Creature[T], Creature[T]]:
        return NotImplemented


class SinglePointCrossover(ABC, Crossover[T]):
    def cross_two(self, a: Creature[T], b: Creature[T]) -> Tuple[Creature[T], Creature[T]]:
        pass  # TODO


class RandomCrossover(ABC, Crossover[T]):
    def cross(self, population: List[Creature[T]]) -> List[Creature[T]]:
        pass  # TODO


class SinglePointRandomCrossover(SinglePointCrossover[T], RandomCrossover[T]):
    pass
