import random
from abc import ABC, abstractmethod
from copy import copy, deepcopy
from typing import TypeVar, Generic, List

from genetwork.genetic.creatures import Creature, SndlibGene, Gene

T = TypeVar('T', bound=Gene)


class Mutation(ABC, Generic[T]):
    @abstractmethod
    def mutate_population(self, creatures: List[Creature[SndlibGene]]) -> List[Creature[SndlibGene]]:
        return NotImplemented

    @abstractmethod
    def mutate_creature(self, creature: Creature[T]) -> Creature[T]:
        return NotImplemented


class SndlibMutation(Mutation[SndlibGene]):
    @abstractmethod
    def mutate_population(self, creatures: List[Creature[SndlibGene]]) -> List[Creature[SndlibGene]]:
        return NotImplemented

    @abstractmethod
    def mutate_creature(self, creature: Creature[SndlibGene]) -> Creature[SndlibGene]:
        return NotImplemented


class DeltaMutation(SndlibMutation):
    def __init__(self, pm: float):
        self.pm = pm

    def mutate_population(self, creatures: List[Creature[SndlibGene]]) -> List[Creature[SndlibGene]]:
        return [self.mutate_creature(c) for c in creatures]

    def mutate_creature(self, creature: Creature[SndlibGene]) -> Creature[SndlibGene]:
        c = copy(creature)
        for i, gene in enumerate(creature.chromosome):
            for j, placement in enumerate(gene.placements):
                for trans_type, nb in placement.quantity_dict.items():
                    if random.uniform(0, 1) < self.pm:
                        c.chromosome = copy(creature.chromosome)
                        c.chromosome[i] = copy(creature.chromosome[i])
                        c.chromosome[i].placements = copy(creature.chromosome[i].placements)
                        c.chromosome[i].placements[j] = deepcopy(creature.chromosome[i].placements[j])
                        c.chromosome[i].placements[j].quantity_dict[trans_type] = max(0, nb + random.choice((-1, 1)))
        return c
