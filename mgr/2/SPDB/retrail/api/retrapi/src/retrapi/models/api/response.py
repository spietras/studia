from pydantic import BaseModel

from retrapi.models.data import Path, Point


class ClosestResponse(BaseModel):
    point: Point


class FindResponse(BaseModel):
    path: Path
    cost: float
