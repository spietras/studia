from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List

from genetwork.genetic.creatures import Creature, SndlibGene, Gene

T = TypeVar('T', bound=Gene)


class PopulationInitializer(ABC, Generic[T]):
    @abstractmethod
    def init(self) -> List[Creature[T]]:
        return NotImplemented


class SndlibPopulationInitializer(PopulationInitializer[SndlibGene]):
    @abstractmethod
    def init(self) -> List[Creature[SndlibGene]]:
        return NotImplemented


class RandomSndlibPopulationInitializer(SndlibPopulationInitializer):
    def init(self) -> List[Creature[SndlibGene]]:
        pass  # TODO
