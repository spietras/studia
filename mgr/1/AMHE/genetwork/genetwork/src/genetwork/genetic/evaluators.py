from abc import ABC, abstractmethod
from typing import TypeVar, Generic, List, Tuple

from genetwork.genetic.creatures import Creature, SndlibGene, Gene
from genetwork.networks import SndlibDemand

T = TypeVar('T', bound=Gene)


class Evaluator(ABC, Generic[T]):
    def evaluate_creature(self, creature: Creature[T]) -> float:
        return self.evaluate(creature)[0]

    def is_creature_valid(self, creature: Creature[T]) -> bool:
        return self.evaluate(creature)[1]

    @abstractmethod
    def evaluate(self, creature: Creature[T]) -> Tuple[float, bool]:
        return NotImplemented


class SndlibEvaluator(Evaluator[SndlibGene]):
    def evaluate_creature(self, creature: Creature[SndlibGene]) -> float:
        return super().evaluate_creature(creature)

    def is_creature_valid(self, creature: Creature[SndlibGene]) -> bool:
        return super().is_creature_valid(creature)

    @abstractmethod
    def evaluate(self, creature: Creature[SndlibGene]) -> Tuple[float, bool]:
        return NotImplemented


class TotalCostWithConstraintsSndlibEvaluator(SndlibEvaluator):
    def __init__(self, demands: List[SndlibDemand], alpha: float, beta: float, lam: int):
        self.demands = demands
        self.alpha = alpha
        self.beta = beta
        self.lam = lam

    def evaluate(self, creature: Creature[SndlibGene]) -> Tuple[float, bool]:
        cost = 0
        links_capacity_used = {}
        is_valid = True

        for gene, demand in zip(creature.chromosome, self.demands):
            supply = 0

            for placement in gene.placements:
                trans_used = 0

                for trans_type, q in placement.quantity_dict.items():
                    trans_used += q
                    cost += q * trans_type.cost * 2  # one transponder on each side
                    supply += q * trans_type.capacity

                for edge in placement.path.edges:
                    links_capacity_used[edge.id] = links_capacity_used.get(edge.id, 0) + trans_used

            # penalty function g for not supplying necessary demand
            g = demand.value - supply
            if g > 0:
                cost += self.alpha * g
                is_valid = False

        # penalty function h for exceeding fiber capacity
        for lambda_q in links_capacity_used.values():
            h = lambda_q - self.lam
            if h > 0:
                cost += self.beta * h
                is_valid = False

        return cost, is_valid
