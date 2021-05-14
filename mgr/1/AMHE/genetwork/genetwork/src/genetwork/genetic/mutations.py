import random
from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List

from genetwork.genetic.creatures import Creature, SndlibGene, Gene

T = TypeVar('T', bound=Gene)


class Mutation(ABC, Generic[T]):
    @abstractmethod
    def mutate_population(self, creatures: List[Creature[SndlibGene]]) -> None:
        return

    @abstractmethod
    def mutate_creature(self, creature: Creature[T]) -> None:
        return


class SndlibMutation(Mutation[SndlibGene]):
    @abstractmethod
    def mutate_population(self, creatures: List[Creature[SndlibGene]]) -> None:
        return

    @abstractmethod
    def mutate_creature(self, creature: Creature[SndlibGene]) -> None:
        return


class DeltaMutation(SndlibMutation):
    def __init__(self, pm: float):
        self.pm = pm

    def mutate_population(self, creatures: List[Creature[SndlibGene]]) -> None:
        for creature in creatures:
            self.mutate_creature(creature)

    def mutate_creature(self, creature: Creature[SndlibGene]) -> None:
        for gene in creature.chromosome:
            for placement in gene.placements:
                for trans_type, nb in placement.quantity_dict.items():
                    if random.uniform(0, 1) < self.pm:
                        if random.uniform(0, 1) < self.pm:
                            placement.quantity_dict[trans_type] = nb + 1
                        else:
                            placement.quantity_dict[trans_type] = nb - 1 if nb > 0 else 0
