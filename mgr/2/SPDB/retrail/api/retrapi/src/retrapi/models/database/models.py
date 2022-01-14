from pydantic import BaseModel


class Point(BaseModel):
    x: float
    y: float


class PathRow(BaseModel):
    x: float
    y: float
    cost: float
