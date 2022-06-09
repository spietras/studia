import copy
import math
from abc import ABC
from typing import Generic, Tuple, TypeVar

from geopy.distance import geodesic
from pydantic import conlist

from retrapi.models.base import BaseModel, GenericModel


class Point(BaseModel):
    x: float
    y: float

    @property
    def tuple(self) -> Tuple[float, float]:
        return self.x, self.y


class Line(BaseModel):
    start: Point
    end: Point

    @property
    def geodesic_distance(self) -> float:
        return geodesic(self.start.tuple, self.end.tuple).meters

    @property
    def euclidean_distance(self) -> float:
        return math.dist(self.start.tuple, self.end.tuple)


class WeightedLine(Line):
    cost: float = 0


T = TypeVar("T", bound=Line)


class BasePath(GenericModel, ABC, Generic[T]):
    lines: conlist(T, min_items=1)

    @property
    def geodesic_distance(self) -> float:
        return sum(line.geodesic_distance for line in self.lines)

    @property
    def euclidean_distance(self) -> float:
        return sum(line.euclidean_distance for line in self.lines)

    @property
    def n_lines(self) -> int:
        return len(self.lines)

    @property
    def n_points(self) -> int:
        return self.n_lines + 1

    def __add__(self, other: "BasePath") -> "BasePath":
        self_end, other_start = self.lines[-1].end, other.lines[0].start
        if self_end == other_start:
            return self.__class__(lines=self.lines + other.lines)
        else:
            return self.__class__(
                lines=(
                    self.lines
                    + [Line(start=self_end, end=other_start)]
                    + other.lines
                )
            )


class Path(BasePath[Line]):
    pass


class WeightedPath(BasePath[WeightedLine]):
    @property
    def cost(self) -> float:
        return sum(line.cost or 0 for line in self.lines)
