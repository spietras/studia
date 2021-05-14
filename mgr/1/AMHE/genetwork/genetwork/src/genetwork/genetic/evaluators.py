from abc import ABC, abstractmethod
from numbers import Real
from typing import TypeVar, Generic, List, Tuple

from genetwork.genetic.creatures import Creature, SndlibGene, Gene
from genetwork.networks import SndlibDemand

T = TypeVar('T', bound=Gene)


class Evaluator(ABC, Generic[T]):
    @abstractmethod
    def evaluate_creature(self, creature: Creature[T]) -> Real:
        return NotImplemented

    @abstractmethod
    def is_creature_valid(self, creature: Creature[T]) -> bool:
        return NotImplemented

    @abstractmethod
    def evaluate(self, creature: Creature[T]) -> Tuple[Real, bool]:
        return NotImplemented


class SndlibEvaluator(Evaluator[SndlibGene]):
    @abstractmethod
    def evaluate_creature(self, creature: Creature[SndlibGene]) -> Real:
        return NotImplemented

    @abstractmethod
    def is_creature_valid(self, creature: Creature[SndlibGene]) -> bool:
        return NotImplemented

    @abstractmethod
    def evaluate(self, creature: Creature[SndlibGene]) -> Tuple[Real, bool]:
        return NotImplemented


class TotalCostWithConstraintsSndlibEvaluator(SndlibEvaluator):
    def __init__(self, demands: List[SndlibDemand], alpha: Real, beta: Real, lam: int):
        self.demands = demands
        self.alpha = alpha
        self.beta = beta
        self.lam = lam

    def evaluate_creature(self, creature: Creature[SndlibGene]) -> Real:
        return self.evaluate(creature)[0]

    def is_creature_valid(self, creature: Creature[SndlibGene]) -> bool:
        return self.evaluate(creature)[1]

    def evaluate(self, creature: Creature[SndlibGene]) -> Tuple[Real, bool]:
        cost = 0
        links_capacity_used = {}
        is_valid = True

        for gene, demand in zip(creature.chromosome, self.demands):
            supply = 0

            for placement in gene.placements:
                trans_used = 0

                for trans_type, nb in placement.quantity_dict.items():
                    trans_used += nb
                    cost += nb * trans_type.cost * 2  # one transponder on each side
                    supply += nb * trans_type.capacity

                for edge in placement.path.edges:
                    links_capacity_used[edge.id] = trans_used \
                        if edge.id not in links_capacity_used \
                        else links_capacity_used[edge.id] + trans_used

            # penalty function g for unnecessary overcapacity
            g = demand.value - supply
            if g > 0:
                cost += self.alpha * g
                is_valid = False

        # penalty function h for exceeding fiber capacity
        for _, lambda_nb in links_capacity_used.items():
            h = lambda_nb - self.lam
            if h > 0:
                cost += self.beta * h
                is_valid = False

        return cost, is_valid
