from functools import reduce
from typing import List, Mapping, Sequence

import numpy as np
from scipy.spatial import ConvexHull
from scipy.spatial.qhull import QhullError
from sqlalchemy import func, select, sql

import retrapi.models.data as data_models
import retrapi.models.database.models as database_models
from retrapi.database import database
from retrapi.models.api.request import FindRequest
from retrapi.models.api.response import FindResponse


def distance(points: np.ndarray, target: np.ndarray) -> np.ndarray:
    return np.linalg.norm(points - target, axis=-1)


def extreme_point(points: np.ndarray, axis: int = -1) -> int:
    return points.argmax(0)[axis]


def furthest_point(points: np.ndarray) -> int:
    if len(points) < 3:
        return len(points) - 1
    try:
        convex_hull = ConvexHull(points)
    except QhullError:
        return extreme_point(points)
    convex_points = points[convex_hull.vertices]
    distances = distance(convex_points, convex_points.mean(0))
    return convex_hull.vertices[distances.argmax()]


def sort_by_distance(points: np.ndarray, starting: int) -> np.ndarray:
    i = starting
    mask = np.ones(len(points), dtype=bool)
    mask[i] = False
    out = points[i, np.newaxis]
    while len(points[mask]) > 0:
        distances = distance(points[mask], points[i])
        i = mask.nonzero()[0][distances.argmin()]
        out = np.vstack((out, points[i]))
        mask[i] = False
    return out


def order_points(
    points: Sequence[data_models.Point],
) -> List[data_models.Point]:
    points = np.array([point.tuple for point in points])
    first_point = furthest_point(points)
    ordered_points = sort_by_distance(points, first_point)
    return [
        data_models.Point(x=point[0], y=point[1]) for point in ordered_points
    ]


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
