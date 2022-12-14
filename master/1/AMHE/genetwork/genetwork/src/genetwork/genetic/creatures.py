from abc import ABC
from typing import List, TypeVar, Generic

from genetwork.networks import SndlibTranspondersPlacement


class Gene(ABC):
    """Just a marker for type bounds."""
    pass


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
