<h1 align="center">genetwork</h1>

<div align="center">

[![Running tests](https://github.com/spietras/genetwork/actions/workflows/test.yml/badge.svg)](https://github.com/spietras/genetwork/actions/workflows/test.yml)
[![Deploying docs](https://github.com/spietras/genetwork/actions/workflows/docs.yml/badge.svg)](https://github.com/spietras/genetwork/actions/workflows/docs.yml)

</div>

---

Satisfying network demands using genetic algorithm.

## Installing

Using ```pip```<sup>*</sup>:

```sh
pip install genetwork
```

<sup><sup>* assuming the authors bothered to release the package on PyPI...</sup></sup>

## Usage as a library

Example usage:

```python
from genetwork.genetic.creatures import SndlibGene
from genetwork.genetic.crossovers import SinglePointRandomCrossover
from genetwork.genetic.evaluators import TotalCostWithConstraintsSndlibEvaluator
from genetwork.genetic.evolution import Evolution, NoChangeStoppingCriterion
from genetwork.genetic.initializers import RandomSndlibPopulationInitializer
from genetwork.genetic.mutations import DeltaMutation
from genetwork.genetic.selections import ThresholdSelection
from genetwork.genetic.successions import BestOverallSuccession
from genetwork.networks import SndlibNetworkXMLParser, SndlibTransponderType, SndlibNode

with open("network.xml") as network_file:
    network = SndlibNetworkXMLParser[SndlibNode, float](max_paths=4).parse(network_file)
    
transponders = [SndlibTransponderType(capacity=10,  cost=1),
                SndlibTransponderType(capacity=40,  cost=3),
                SndlibTransponderType(capacity=100, cost=5)]

initializer = RandomSndlibPopulationInitializer(pop_size=100, 
                                                trans_types=transponders, 
                                                demands=network.demands, 
                                                max_trans=1, 
                                                succ_prob=0.9)
evaluator = TotalCostWithConstraintsSndlibEvaluator(demands=network.demands, 
                                                    alpha=25, 
                                                    beta=7.5, 
                                                    lam=96)
selection = ThresholdSelection(evaluator=evaluator, 
                               threshold=0.75, 
                               size_factor=2)
crossover = SinglePointRandomCrossover(pc=0.99)
mutation = DeltaMutation(pm=0.002)
succession = BestOverallSuccession(evaluator=evaluator)

evolution = Evolution[SndlibGene](initializer, evaluator, selection, crossover, mutation, succession)
stop_criterion = NoChangeStoppingCriterion()

while not stop_criterion.should_stop(evolution):
    evolution.step()
```

## Usage as a command line tool

```sh
$ genetwork network.xml -o results.json
Generation number: 1, best score: 82030.0, best valid: False
...
```