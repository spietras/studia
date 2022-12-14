import random
from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List

from genetwork.genetic.creatures import Creature, SndlibGene, Gene
from genetwork.networks import SndlibTransponderType, SndlibDemand, SndlibTranspondersPlacement
from scipy.stats import geom

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
    def __init__(self,
                 pop_size: int,
                 trans_types: List[SndlibTransponderType],
                 demands: List[SndlibDemand],
                 max_transponders: int = 0,
                 succ_prob: float = 0.75) -> None:
        self.pop_size = pop_size
        self.trans_types = trans_types
        self.demands = demands
        self.max_transponders = max_transponders
        self.succ_prob = succ_prob
        self.probs = [geom.pmf(i + 1, self.succ_prob) for i in range(self.max_transponders + 1)]

    def init(self) -> List[Creature[SndlibGene]]:
        return [Creature([self.create_random_gene(d) for d in self.demands]) for _ in range(self.pop_size)]

    def create_random_gene(self, demand: SndlibDemand) -> SndlibGene:
        placements = []
        for path in demand.admissible_paths:
            quantity_dict = {
                trans_type: random.choices(range(self.max_transponders + 1), self.probs)[0]
                for trans_type in self.trans_types
            }
            placements.append(SndlibTranspondersPlacement(path, quantity_dict))
        return SndlibGene(placements)
