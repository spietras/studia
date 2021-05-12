from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List

from genetwork.genetic.creatures import Creature, Gene

T = TypeVar('T', bound=Gene)


class Selection(ABC, Generic[T]):
    @abstractmethod
    def select(self, population: List[Creature[T]]) -> List[Creature[T]]:
        return NotImplemented


class RankSelection(Selection[T]):
    def select(self, population: List[Creature[T]]) -> List[Creature[T]]:
        pass  # TODO
