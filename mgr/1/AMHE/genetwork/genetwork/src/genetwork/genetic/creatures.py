from abc import ABC, abstractmethod
from typing import List, Any, TypeVar, Generic

from genetwork.networks import SndlibTranspondersPlacement


class Gene(ABC):
    @abstractmethod
    def value(self) -> Any:
        return NotImplemented

    @abstractmethod
    def is_valid(self) -> bool:
        return NotImplemented


T = TypeVar('T', bound=Gene)


class Creature(ABC, Generic[T]):
    chromosome: List[T]

    def __init__(self, chromosome: List[T]) -> None:
        super().__init__()
        self.chromosome = chromosome


class SndlibGene(Gene):
    def __init__(self, placements: List[SndlibTranspondersPlacement]) -> None:
        super().__init__()
        self.placements = placements

    def value(self) -> Any:
        return self.placements

    def is_valid(self) -> bool:
        pass  # TODO
