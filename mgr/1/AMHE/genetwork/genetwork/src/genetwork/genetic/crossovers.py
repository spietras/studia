import random
from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List, Tuple

from genetwork.genetic.creatures import Creature, Gene

T = TypeVar('T', bound=Gene)


class Crossover(ABC, Generic[T]):
    @abstractmethod
    def cross_population(self, population: List[Creature[T]]) -> List[Creature[T]]:
        return NotImplemented

    @abstractmethod
    def cross_creatures(self,
                        creature_first: Creature[T],
                        creature_second: Creature[T]) -> Tuple[Creature[T], Creature[T]]:
        return NotImplemented


class SinglePointRandomCrossover(Crossover[T]):
    def __init__(self, pc: float):
        self.pc = pc

    def cross_population(self, population: List[Creature[T]]) -> List[Creature[T]]:
        if len(population) % 2 == 0:
            pop_size = len(population)
            crossed_pop = []
        else:
            pop_size = len(population) - 1
            crossed_pop = [population[-1]]

        pairs = random.sample(range(0, pop_size), k=pop_size)

        for idx in range(0, len(pairs), 2):
            first, second = self.cross_creatures(population[pairs[idx]], population[pairs[idx+1]])
            crossed_pop.extend([first, second])

        return crossed_pop

    def cross_creatures(self,
                        creature_first: Creature[T],
                        creature_second: Creature[T]) -> Tuple[Creature[T], Creature[T]]:
        if random.uniform(0, 1) < self.pc:
            cross_point = random.randint(0, len(creature_first.chromosome))

            crossed_first = Creature(creature_first.chromosome[:cross_point] +
                                     creature_second.chromosome[cross_point:])
            crossed_second = Creature(creature_second.chromosome[:cross_point] +
                                      creature_first.chromosome[cross_point:])

            return crossed_first, crossed_second
        else:
            return creature_first, creature_second
