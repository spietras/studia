"""Module for networks code.

Example usage:

    with open(network_path) as network:
        g = SndlibNetworkXMLParser[str, Real]().parse(network)
    print(str(g))

"""

from abc import ABC, abstractmethod
from copy import deepcopy
from numbers import Real
from typing import Hashable, TypeVar, Generic, Tuple, List, Dict, Any, Iterable, Union, Optional, TextIO

from lxml import objectify
from lxml.objectify import ObjectifiedElement
from networkx import Graph, nx

H = TypeVar('H', bound=Hashable)  # type for node
R = TypeVar('R', bound=Real)  # typed for demand values


class Serializable(ABC):
    """Convertible to simpler type representation."""

    @abstractmethod
    def serialize(self) -> Any:
        pass

    def __str__(self) -> str:
        return str(self.serialize())


class TEdge(Serializable, Generic[H]):
    """Generic typed edge."""

    def __init__(self, source: H, target: H) -> None:
        super().__init__()
        self.source = source
        self.target = target

    def serialize(self) -> List:
        return [self.source, self.target]


class TPath(Serializable, Generic[H]):
    """Generic typed path."""

    def __init__(self, edges: List[TEdge[H]]) -> None:
        super().__init__()
        edge_pairs = [self._redirect_edges(e1, e2) for e1, e2 in zip(edges, edges[1:])]
        self.edges = [e1 for e1, e2 in edge_pairs] + [edge_pairs[-1][1]] if edge_pairs else edges
        if not self._check_path(self.edges):
            raise ValueError("Invalid path")
        self.nodes = [edge.source for edge in self.edges] + [self.edges[-1].target]

    @staticmethod
    def _redirect_edges(e1: TEdge[H], e2: TEdge[H]) -> Tuple[TEdge[H], TEdge[H]]:
        if e1.source == e2.source:
            n1, n2 = (e1.target, e1.source), (e2.source, e2.target)
        elif e1.source == e2.target:
            n1, n2 = (e1.target, e1.source), (e2.target, e2.source)
        elif e1.target == e2.source:
            n1, n2 = (e1.source, e1.target), (e2.source, e2.target)
        elif e1.target == e2.target:
            n1, n2 = (e1.source, e1.target), (e2.target, e2.source)
        else:
            raise ValueError("Edges don't have a common node")
        e1, e2 = deepcopy(e1), deepcopy(e2)
        e1.source, e1.target, e2.source, e2.target = n1[0], n1[1], n2[0], n2[1]
        return e1, e2

    @staticmethod
    def _check_path(edges: List[TEdge[H]]) -> bool:
        return all(e1.target == e2.source for e1, e2 in zip(edges, edges[1:]))

    def has_node(self, n: H) -> bool:
        return n in self.nodes

    def has_edge(self, u: H, v: H) -> bool:
        zipped = list(zip(self.nodes, self.nodes[1:]))
        return (u, v) in zipped or (v, u) in zipped

    def serialize(self) -> List[H]:
        return self.nodes


class TGraph(Serializable, Graph, Generic[H]):
    """Generic typed graph."""

    def __init__(self, incoming_graph_data=None, **attr) -> None:
        super().__init__(incoming_graph_data, **attr)
        if any(not isinstance(x, Hashable) for x in self.nodes):
            raise ValueError("All nodes must be hashable")
        if not all(type(x) is type(next(n for n in self.nodes)) for x in self.nodes):
            raise ValueError("All nodes must be the same type")

    @property
    def adj(self) -> Dict[H, Dict[H, Any]]:
        return super().adj

    def __iter__(self) -> Iterable[H]:
        return super().__iter__()

    def __contains__(self, n: H) -> bool:
        return super().__contains__(n)

    def __getitem__(self, n: H) -> Dict[H, Any]:
        return super().__getitem__(n)

    def add_node(self, node_for_adding: H, **attr) -> None:
        super().add_node(node_for_adding, **attr)

    def add_nodes_from(self, nodes_for_adding: Iterable[H], **attr) -> None:
        super().add_nodes_from(nodes_for_adding, **attr)

    def remove_node(self, n: H) -> None:
        return super().remove_node(n)

    def remove_nodes_from(self, nodes: Iterable[H]) -> None:
        super().remove_nodes_from(nodes)

    @property
    def nodes(self) -> Iterable[H]:
        return super().nodes

    def has_node(self, n: H) -> bool:
        return super().has_node(n)

    def add_edge(self, u_of_edge: H, v_of_edge: H, **attr) -> None:
        super().add_edge(u_of_edge, v_of_edge, **attr)

    def add_edges_from(self, ebunch_to_add: Union[Iterable[Tuple[H, H]], Iterable[Tuple[H, H, Dict]]], **attr) -> None:
        return super().add_edges_from(ebunch_to_add, **attr)

    def add_weighted_edges_from(self, ebunch_to_add: Union[Iterable[Tuple[H, H]], Iterable[Tuple[H, H, Real]]],
                                weight: str = "weight", **attr) -> None:
        super().add_weighted_edges_from(ebunch_to_add, weight, **attr)

    def remove_edge(self, u: H, v: H) -> None:
        return super().remove_edge(u, v)

    def remove_edges_from(self, ebunch: Union[Iterable[Tuple[H, H]], Iterable[Tuple[H, H, Any]]]) -> None:
        super().remove_edges_from(ebunch)

    def update(self,
               edges: Optional[Union['TGraph[H]', Graph, Iterable[Tuple[H, H]], Iterable[Tuple[H, H, Dict]]]] = None,
               nodes: Optional[Iterable[H]] = None) -> None:
        return super().update(edges, nodes)

    def has_edge(self, u: H, v: H) -> bool:
        return super().has_edge(u, v)

    def neighbors(self, n: H) -> Iterable[H]:
        return super().neighbors(n)

    @property
    def edges(self) -> Union[Iterable[Tuple[H, H]], Iterable[Tuple[H, H, Dict]]]:
        return super().edges

    def get_edge_data(self, u: H, v: H, default=None) -> Dict:
        return super().get_edge_data(u, v, default)

    def adjacency(self) -> Iterable[Tuple[H, Dict[H, Dict]]]:
        return super().adjacency()

    def subgraph(self, nodes: Iterable[H]) -> 'TGraph[H]':
        g = super().subgraph(nodes)
        g.__class__ = TGraph[H]
        return g

    def edge_subgraph(self, edges: Iterable[Tuple[H, H]]) -> 'TGraph[H]':
        g = super().edge_subgraph(edges)
        g.__class__ = TGraph[H]
        return g

    def number_of_edges(self, u=None, v=None):
        return super().number_of_edges(u, v)

    def has_path(self, u: H, v: H) -> bool:
        return u in self and v in self and nx.has_path(self, u, v)

    def is_valid_path(self, path: Union[TPath[H], List[H]]) -> bool:
        if isinstance(path, TPath):
            path = path.nodes
        return all(map(self.has_edge, path, path[1:]))

    def serialize(self) -> Dict:
        return {
            "name": self.name,
            "nodes": list(self.nodes),
            "edges": list(self.edges)
        }


class TDemand(Serializable, Generic[H, R]):
    """Generic typed demand."""

    def __init__(self, source: H, target: H, value: R, admissible_paths: Optional[List[TPath[H]]] = None) -> None:
        super().__init__()
        if admissible_paths is not None:
            if not admissible_paths:
                raise ValueError("Admissible paths can't be empty")
            if any(path.nodes[0] != source or path.nodes[-1] != target for path in admissible_paths):
                raise ValueError("All paths must begin in source node and end in target node")
        self.source = source
        self.target = target
        self.value = value
        self.admissible_paths = admissible_paths

    def nodes(self) -> Tuple[H, H]:
        return self.source, self.target

    def serialize(self) -> Dict:
        return {
            "source": self.source,
            "target": self.target,
            "value": self.value,
            "admissible_paths": None if self.admissible_paths is None else [path.serialize() for path in
                                                                            self.admissible_paths]
        }


class SndlibNode(Serializable, Generic[H]):
    def __init__(self, id: H) -> None:
        super().__init__()
        self.id = id

    def __hash__(self) -> int:
        return hash(self.id)

    def __eq__(self, o: object) -> bool:
        if not isinstance(o, SndlibNode):
            return NotImplemented
        return self.id == o.id

    def serialize(self) -> Any:
        return self.id


class SndlibLink(TEdge[SndlibNode[H]]):
    def __init__(self, id: str, source: SndlibNode[H], target: SndlibNode[H]) -> None:
        super().__init__(source, target)
        self.id = id

    def serialize(self) -> List:
        return [node.serialize() for node in super().serialize()]


class SndlibPath(TPath[SndlibNode[H]]):
    def __init__(self, id: str, links: List[SndlibLink[H]]) -> None:
        super().__init__(links)
        self.id = id

    def serialize(self) -> List[H]:
        return [node.serialize() for node in super().serialize()]


class SndlibGraph(TGraph[SndlibNode[H]]):
    def __init__(self, links: List[SndlibLink[H]], **attr) -> None:
        super().__init__([(link.source, link.target) for link in links], **attr)
        self.links = links

    def serialize(self) -> Dict:
        d = super().serialize()
        d.pop("edges")
        return d | {"nodes": [node.serialize() for node in self.nodes],
                    "links": [link.serialize() for link in self.links]}


class SndlibDemand(TDemand[SndlibNode[H], R]):
    def __init__(self, id: str, source: SndlibNode[H], target: SndlibNode[H], value: R,
                 admissible_paths: Optional[List[SndlibPath[H]]] = None) -> None:
        super().__init__(source, target, value, admissible_paths)
        self.id = id

    def serialize(self) -> Dict:
        return super().serialize() | {"source": self.source.serialize(), "target": self.target.serialize()}


class SndlibNetwork(Serializable, Generic[H, R]):
    """Network for Sndlib problems."""

    def __init__(self, graph: SndlibGraph[H],
                 demands: List[SndlibDemand[H, R]]) -> None:
        super().__init__()
        if any(not graph.has_path(demand.source, demand.target) for demand in demands):
            raise ValueError("All demands must be between reachable nodes")
        if any(not graph.is_valid_path(path)
               for demand in demands if demand.admissible_paths for path in demand.admissible_paths):
            raise ValueError("All paths must contain existing edges")
        self.graph = graph
        self.demands = demands

    def subgraph(self, nodes: Iterable[H]) -> 'SndlibNetwork[H, R]':
        g = self.graph.subgraph(nodes)
        d = [demand for demand in self.demands if g.has_path(demand.source, demand.target)]
        for demand in d:
            d.admissible_paths = [path for path in demand.admissible_paths if g.is_valid_path(path)]
        return SndlibNetwork(g, d)

    def serialize(self) -> Dict:
        return {
            "graph": self.graph.serialize(),
            "demands": [d.serialize() for d in self.demands]
        }


class SndlibNetworkParser(ABC, Generic[H, R]):
    @abstractmethod
    def parse(self, *args) -> SndlibNetwork[H, R]:
        return NotImplemented


class SndlibNetworkXMLParser(SndlibNetworkParser[H, R]):
    def parse(self, source: TextIO) -> SndlibNetwork[H, R]:
        root = objectify.parse(source).getroot()
        graph = self._get_graph(root)
        return SndlibNetwork(graph, self._get_demands(root, graph))

    @staticmethod
    def _get_graph(root: ObjectifiedElement) -> SndlibGraph[H]:
        return SndlibGraph([SndlibLink(link.get("id"),
                                       SndlibNode(link.source.pyval),
                                       SndlibNode(link.target.pyval)) for link in root.networkStructure.links.link])

    @staticmethod
    def _get_demands(root: ObjectifiedElement, graph: SndlibGraph[H]) -> List[SndlibDemand[H, R]]:
        def get_path(path_root: ObjectifiedElement, graph: SndlibGraph[H]) -> SndlibPath[H]:
            links_map = {link.id: link for link in graph.links}
            return SndlibPath(path_root.get("id"), [links_map[link_id.pyval] for link_id in path_root.linkId])

        def get_paths(demand_root: ObjectifiedElement, graph: SndlibGraph[H]) -> Optional[List[SndlibPath[H]]]:
            return [get_path(path, graph) for path
                    in demand_root.admissiblePaths.admissiblePath] if hasattr(demand_root, "admissiblePaths") else None

        return [SndlibDemand(demand.get("id"),
                             SndlibNode(demand.source.pyval),
                             SndlibNode(demand.target.pyval),
                             demand.demandValue.pyval,
                             get_paths(demand, graph)) for demand in root.demands.demand]


class SndlibTransponderType:
    def __init__(self, capacity: Real, cost: Real) -> None:
        super().__init__()
        self.capacity = capacity
        self.cost = cost

    def __hash__(self) -> int:
        return hash(self.capacity)


class SndlibTranspondersPlacement:
    def __init__(self, path: SndlibPath, quantity_dict: Dict[SndlibTransponderType, int]) -> None:
        super().__init__()
        self.path = path
        self.quantity_dict = quantity_dict

    def quantity_of(self, transponder_type: SndlibTransponderType):
        return self.quantity_dict[transponder_type]