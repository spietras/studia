from typing import Literal

from pydantic import conlist

from retrapi.models.base import BaseModel
from retrapi.models.data import Point


class ClosestRequest(BaseModel):
    point: Point


class FindRequest(BaseModel):
    points: conlist(Point, min_items=2)
    mode: Literal["distance", "time"] = "distance"
