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
        population = random.sample(population, len(population))  # shuffle
        children = [] if len(population) % 2 == 0 else [population.pop()]
        for p1, p2 in zip(population[0::2], population[1::2]):
            c1, c2 = self.cross_creatures(p1, p2) if random.random() < self.pc else (p1, p2)
            children.extend((c1, c2))
        return children

    def cross_creatures(self, p1: Creature[T], p2: Creature[T]) -> Tuple[Creature[T], Creature[T]]:
        cross_point = random.randint(0, len(p1.chromosome))
        c1 = Creature(p1.chromosome[:cross_point] + p2.chromosome[cross_point:])
        c2 = Creature(p2.chromosome[:cross_point] + p1.chromosome[cross_point:])
        return c1, c2
