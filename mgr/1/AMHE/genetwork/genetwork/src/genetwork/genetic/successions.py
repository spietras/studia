from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List

from genetwork.genetic.creatures import Creature, Gene

T = TypeVar('T', bound=Gene)


class Succession(ABC, Generic[T]):
    @abstractmethod
    def pick(self, parents: List[Creature[T]], offspring: List[Creature[T]]) -> List[Creature[T]]:
        return NotImplemented


class EliteSuccession(Succession[T]):
    def pick(self, parents: List[Creature[T]], offspring: List[Creature[T]]) -> List[Creature[T]]:
        pass  # TODO
