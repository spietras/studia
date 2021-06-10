from abc import ABC, abstractmethod

from genetwork.genetic.evolution import Evolution


class EvolutionLogger(ABC):
    @abstractmethod
    def log(self, evolution: Evolution):
        return NotImplemented


class EvolutionConsoleLogger(EvolutionLogger):
    def log(self, evolution: Evolution):
        score, valid = evolution.evaluator.evaluate(evolution.best_creature())
        print(f"Generation number: {evolution.n_generation}, best score: {score}, best valid: {valid}")
