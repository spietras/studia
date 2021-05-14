import random
from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List

from genetwork.genetic.creatures import Creature, SndlibGene, Gene
from genetwork.networks import SndlibTransponderType, SndlibDemand, SndlibTranspondersPlacement

T = TypeVar('T', bound=Gene)

MAX_TRANS_NB = 1


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
                 demands: List[SndlibDemand]) -> None:
        self.pop_size = pop_size
        self.trans_types = trans_types
        self.demands = demands

    def init(self) -> List[Creature[SndlibGene]]:
        population = []

        for size in range(self.pop_size):
            genes = []

            for demand in self.demands:
                gene = self.create_random_gene(demand)
                genes.append(gene)

            population.append(Creature(genes))

        return population

    def create_random_gene(self, demand: SndlibDemand) -> SndlibGene:
        placements = []

        for path in demand.admissible_paths:
            rand_trans_types_nb = random.choices(range(0, MAX_TRANS_NB+1), k=len(self.trans_types))
            quantity_dict = {}

            for trans_type, nb in zip(self.trans_types, rand_trans_types_nb):
                quantity_dict[trans_type] = nb

            placements.append(SndlibTranspondersPlacement(path, quantity_dict))

        return SndlibGene(placements)
