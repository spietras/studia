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

    def step(self) -> None:
        selected = self.selection.select(self.population)
        children = self.crossover.cross(selected)
        children = [self.mutation.mutate(c) for c in children]
        self.population = self.succession.pick(self.population, children)


class StoppingCriteria(ABC):
    @abstractmethod
    def should_stop(self, e: Evolution) -> bool:
        return NotImplemented


class MaxStepsStoppingCriteria(StoppingCriteria):
    def should_stop(self, e: Evolution) -> bool:
        pass  # TODO
