from typing import (
    Callable,
    Container,
    Dict,
    Iterable,
    List,
    Sequence,
    Tuple,
)

import networkx as nx
import nltk
import numpy as np
from nltk import BigramAssocMeasures, BigramCollocationFinder, word_tokenize

nltk.download('punkt', quiet=True)


def tokenize(
        texts: Iterable[str]
) -> Tuple[List[List[str]], List[str]]:
    tokens, vocabulary = [], set()
    for text in texts:
        tokens.append(word_tokenize(text))
        vocabulary.update(tokens[-1])
    return tokens, sorted(vocabulary)


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
    nodes = [
        (token1, token2, {'weight': score})
        for (token1, token2), score in collocations
    ]
    graph = nx.DiGraph(nodes)
    graph.add_nodes_from(vocabulary)
    return graph


def weights_matrix(graph: nx.Graph, nodes: Sequence[str]) -> np.ndarray:
    return nx.convert_matrix.to_numpy_array(graph, nodelist=nodes)


def matrix_cosine(x: np.ndarray, y: np.ndarray) -> np.ndarray:
    norm = np.linalg.norm(x, axis=1) * np.linalg.norm(y, axis=1)
    sums = np.einsum('ij,ij->i', x, y)
    return np.divide(sums, norm, out=np.zeros_like(sums), where=norm != 0)


def cosine_similarities(
        graph1: nx.DiGraph,
        graph2: nx.DiGraph,
        nodes_order: Sequence[str]
) -> Dict[str, float]:
    if graph1.nodes != graph2.nodes:
        raise ValueError("Graphs must have the same nodes")
    scores = matrix_cosine(
        weights_matrix(graph1, nodes_order),
        weights_matrix(graph2, nodes_order)
    )
    return {
        node: score
        for node, score in zip(nodes_order, scores)
    }
