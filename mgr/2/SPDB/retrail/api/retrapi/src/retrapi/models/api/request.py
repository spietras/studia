from pydantic import BaseModel, conlist

from retrapi.models.data import Point


class ClosestRequest(BaseModel):
    point: Point


class FindRequest(BaseModel):
    points: conlist(Point, min_items=2)
