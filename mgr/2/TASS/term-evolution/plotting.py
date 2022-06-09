from tempfile import NamedTemporaryFile
from typing import Any, Callable, Dict, Literal, Optional, Union
from urllib.parse import quote

import networkx as nx
from IPython.lib.display import IFrame
from matplotlib import pyplot as plt
from pyvis.network import Network

default_draw_kwargs = {
    "with_labels": True, "font_weight": "bold"
}

default_edge_kwargs = {}


def draw_matplotlib_graph(
        graph: nx.DiGraph,
        ax: Optional[plt.Axes] = None,
        size: int = 10,
        layout_fn: Callable[[nx.Graph], Dict] = nx.random_layout,
        draw_kwargs: Optional[Dict[str, Any]] = None,
        edge_kwargs: Optional[Dict[str, Any]] = None, ) -> None:
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


def export_to_html(graph) -> str:
    net = Network(directed=True, notebook=True)
    net.from_nx(graph)
    with NamedTemporaryFile(mode="w+t", suffix=".html") as file:
        net.show(file.name)
        file.flush()
        file.seek(0)
        return file.read()


def draw_from_html(
        html: str,
        width: Union[int, str],
        height: Union[int, str]
) -> IFrame:
    html = 'data:text/html;charset=utf-8,' + quote(html)
    return IFrame(html, width=width, height=height)


def draw_pyvis_graph(
        graph: nx.DiGraph,
        label_attribute: Optional[str] = None,
        label_function: Callable[[Any], str] = lambda x: x,
        width_attribute: Optional[str] = None,
        width_function: Callable[[Any], str] = lambda x: x
) -> IFrame:
    graph = graph.copy()
    if label_attribute is not None:
        nx.set_edge_attributes(
            graph,
            {
                edge: label_function(attribute)
                for edge, attribute in
                nx.get_edge_attributes(graph, label_attribute).items()
            },
            name="label"
        )
    if width_attribute is not None:
        nx.set_edge_attributes(
            graph,
            {
                edge: width_function(attribute)
                for edge, attribute in
                nx.get_edge_attributes(graph, width_attribute).items()
            },
            name="value"
        )
    html = export_to_html(graph)
    return draw_from_html(html, width=510, height=510)


def draw_graph(
        graph: nx.DiGraph,
        backend: Literal['matplotlib', 'pyvis'] = 'pyvis',
        **kwargs
) -> Optional[IFrame]:
    if backend == 'pyvis':
        return draw_pyvis_graph(graph, **kwargs)
    if backend == 'matplotlib':
        return draw_matplotlib_graph(graph, **kwargs)
    raise ValueError(f"Unsupported drawing backend: {backend}")
