import math
from typing import Tuple

from geopy.distance import geodesic
from pydantic import BaseModel, conlist


class Point(BaseModel):
    x: float
    y: float

    @property
    def tuple(self) -> Tuple[float, float]:
        return self.x, self.y


class Line(BaseModel):
    start: Point
    end: Point
    cost: float = 0

    @property
    def geodesic_distance(self) -> float:
        return geodesic(self.start.tuple, self.end.tuple).meters

    @property
    def euclidean_distance(self) -> float:
        return math.dist(self.start.tuple, self.end.tuple)


class Path(BaseModel):
    lines: conlist(Line, min_items=1)

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

    @property
    def cost(self) -> float:
        return sum(line.cost or 0 for line in self.lines)

    def __add__(self, other: "Path"):
        self_end, other_start = self.lines[-1].end, other.lines[0].start
        if self_end == other_start:
            return Path(lines=self.lines + other.lines)
        else:
            return Path(
                lines=self.lines
                + [Line(start=self_end, end=other_start)]
                + other.lines
            )
