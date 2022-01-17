from pydantic import validator

from retrapi.models.base import BaseModel
from retrapi.models.data import Path, Point, WeightedPath


class BorderResponse(BaseModel):
    border: Path

    @validator("border")
    def border_closes(cls, border: Path):
        if border.lines[0].start != border.lines[-1].end:
            raise ValueError("Border is not closed")
        return border


class ClosestResponse(BaseModel):
    point: Point


class FindResponse(BaseModel):
    path: WeightedPath
    cost: float
