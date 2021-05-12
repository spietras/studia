from abc import ABC, abstractmethod
from typing import TypeVar, Generic

from genetwork.genetic.creatures import Creature, SndlibGene, Gene

T = TypeVar('T', bound=Gene)


class Mutation(ABC, Generic[T]):
    @abstractmethod
    def mutate(self, a: Creature[T]) -> Creature[T]:
        return NotImplemented


class SndlibMutation(Mutation[SndlibGene]):
    @abstractmethod
    def mutate(self, a: Creature[SndlibGene]) -> Creature[SndlibGene]:
        return NotImplemented


class DeltaMutation(SndlibMutation):
    def mutate(self, a: Creature[SndlibGene]) -> Creature[SndlibGene]:
        pass  # TODO
