from abc import ABC, abstractmethod
from typing import TypeVar, Generic

from genetwork.genetic.creatures import Gene
from genetwork.genetic.crossovers import Crossover
from genetwork.genetic.evaluators import Evaluator
from genetwork.genetic.initializers import PopulationInitializer
from genetwork.genetic.mutations import Mutation
from genetwork.genetic.selections import Selection
from genetwork.genetic.successions import Succession

T = TypeVar('T', bound=Gene)


class Evolution(Generic[T]):
    def __init__(self,
                 initializer: PopulationInitializer[T],
                 evaluator: Evaluator[T],
                 selection: Selection[T],
                 crossover: Crossover[T],
                 mutation: Mutation[T],
                 succession: Succession[T]) -> None:
        super().__init__()
        self.evaluator = evaluator
        self.selection = selection
        self.crossover = crossover
        self.mutation = mutation
        self.succession = succession
        self.population = initializer.init()
        self.n_generation = 0

    def step(self) -> None:
        selected = self.selection.select(self.population)
        children = self.crossover.cross_population(selected)
        children = self.mutation.mutate_population(children)
        self.population = self.succession.pick(self.population, children)
        self.n_generation += 1


class StoppingCriterion(ABC):
    @abstractmethod
    def should_stop(self, e: Evolution) -> bool:
        return NotImplemented


class MaxStepsStoppingCriterion(StoppingCriterion):
    def __init__(self, n: int) -> None:
        super().__init__()
        self.n = n

    def should_stop(self, e: Evolution) -> bool:
        return e.n_generation >= self.n


class NoChangeStoppingCriterion(StoppingCriterion):
    def __init__(self, over: int = 10) -> None:
        super().__init__()
        self.over = over
        self.previous = []

    def should_stop(self, e: Evolution) -> bool:
        score = e.evaluator.evaluate_creature(e.population[0])
        self.previous = self.previous if len(self.previous) < self.over else self.previous[1:]
        self.previous = self.previous + [score]
        return len(self.previous) == self.over and all(x == score for x in self.previous)


class FirstValidStoppingCriterion(StoppingCriterion):
    def should_stop(self, e: Evolution) -> bool:
        return e.evaluator.is_creature_valid(e.population[0])
