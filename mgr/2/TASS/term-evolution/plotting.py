from typing import Any, Callable, Dict, Optional

import networkx as nx
from matplotlib import pyplot as plt

default_draw_kwargs = {
    "with_labels": True,
    "font_weight": "bold"
}

default_edge_kwargs = {}


def draw_graph(
        graph: nx.DiGraph,
        ax: Optional[plt.Axes] = None,
        size: int = 10,
        layout_fn: Callable[[nx.Graph], Dict] = nx.kamada_kawai_layout,
        draw_kwargs: Optional[Dict[str, Any]] = None,
        edge_kwargs: Optional[Dict[str, Any]] = None,
) -> None:
    ax = ax if ax is not None else plt.subplots(figsize=(size, size))[1]
    draw_kwargs = default_draw_kwargs | (draw_kwargs or {})
    edge_kwargs = default_edge_kwargs | (edge_kwargs or {})
    pos = layout_fn(graph)
    nx.draw(graph, pos=pos, ax=ax, **draw_kwargs)
    nx.draw_networkx_edge_labels(
        graph,
        pos=pos,
        edge_labels={edge: f"{weight:.2f}" for edge, weight in
                     nx.get_edge_attributes(graph, 'weight').items()},
        ax=ax,
        **edge_kwargs
    )
