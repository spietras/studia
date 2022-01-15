from functools import reduce
from typing import List, Mapping, Sequence

import networkx as nx
from scipy.spatial import distance
from sqlalchemy import func, select, sql

import retrapi.models.data as data_models
import retrapi.models.database.models as database_models
from retrapi.database import database
from retrapi.models.api.request import FindRequest
from retrapi.models.api.response import FindResponse


def order_points(
    points: Sequence[data_models.Point],
) -> List[data_models.Point]:
    graph = nx.Graph()
    coords = [point.tuple for point in points]
    distances = distance.cdist(coords, coords, "euclidean")
    for i in range(len(distances)):
        for j in range(i, len(distances)):
            graph.add_edge(i, j, weight=distances[i, j])
    order = nx.algorithms.approximation.traveling_salesman_problem(
        graph, cycle=False
    )
    return [points[i] for i in order]


def create_find_path_query(
    start: data_models.Point, end: data_models.Point
) -> sql.ClauseElement:
    return select(
        func.find_path(start.x, start.y, end.x, end.y).table_valued(
            "x", "y", "cost"
        )
    )


def parse_find_path_result(record: Mapping) -> database_models.PathRow:
    return database_models.PathRow.parse_obj(record)


async def query_find_path(
    start: data_models.Point, end: data_models.Point
) -> List[database_models.PathRow]:
    query = create_find_path_query(start, end)
    results = await database.fetch_all(query)
    return [parse_find_path_result(result) for result in results]


def parse_path(path: List[database_models.PathRow]) -> data_models.Path:
    return data_models.Path(
        lines=[
            data_models.Line(
                start=data_models.Point(x=p1.x, y=p1.y),
                end=data_models.Point(x=p2.x, y=p2.y),
                cost=p2.cost - p1.cost,
            )
            for p1, p2 in zip(path, path[1:])
        ]
    )


def combine_paths(paths: Sequence[data_models.Path]) -> data_models.Path:
    return reduce((lambda x, y: x + y), paths)


async def find_path(points: Sequence[data_models.Point]) -> data_models.Path:
    paths = []
    for p1, p2 in zip(points, points[1:]):
        path = await query_find_path(p1, p2)
        paths.append(parse_path(path))
    return combine_paths(paths)


async def handle(request: FindRequest) -> FindResponse:
    ordered_points = order_points(request.points)
    path = await find_path(ordered_points)
    return FindResponse(path=path, cost=path.cost)
