from typing import (
    Callable,
    Container,
    Dict,
    Sequence,
    Tuple,
)

import networkx as nx
import numpy as np
from nltk import BigramAssocMeasures, BigramCollocationFinder
from tqdm.auto import tqdm


def score_collocations(
        tokens: Sequence[str],
        measure: Callable = BigramAssocMeasures.jaccard
) -> Dict[Tuple[str, str], float]:
    finder = BigramCollocationFinder.from_words(tokens)
    return finder.score_ngrams(measure)


def make_graph(
        tokens: Sequence[str],
        vocabulary: Container[str]
) -> nx.DiGraph:
    collocations = score_collocations(tokens)
    edges = [
        (token1, token2, {'weight': score})
        for (token1, token2), score in collocations
    ]
    graph = nx.DiGraph(edges)
    graph.add_nodes_from(vocabulary)
    return graph.subgraph(vocabulary)


def adjacency_vector(
        graph: nx.DiGraph,
        node: str,
        nodes_order: Sequence[str]
) -> np.ndarray:
    index = {n: i for i, n in enumerate(nodes_order)}
    array = np.zeros(len(nodes_order))
    for neighbour, attribute in graph[node].items():
        array[index[neighbour]] = attribute["weight"]
    return array


def vector_cosine(x: np.ndarray, y: np.ndarray) -> float:
    norm = np.linalg.norm(x) * np.linalg.norm(y)
    return x @ y.T / norm if norm != 0.0 else 0.0


def cosine_similarities(
        graph1: nx.DiGraph,
        graph2: nx.DiGraph,
        nodes_order: Sequence[str],
        progress: bool = True
) -> Dict[str, float]:
    if graph1.nodes != graph2.nodes:
        raise ValueError("Graphs must have the same nodes")
    return {
        node: vector_cosine(
            adjacency_vector(graph1, node, nodes_order),
            adjacency_vector(graph2, node, nodes_order)
        )
        for node in tqdm(nodes_order, disable=not progress)
    }
