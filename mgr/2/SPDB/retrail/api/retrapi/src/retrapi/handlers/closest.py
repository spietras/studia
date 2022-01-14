from typing import Mapping

from sqlalchemy import func, select, sql

import retrapi.models.data as data_models
import retrapi.models.database.models as database_models
from retrapi.database import database
from retrapi.models.api.request import ClosestRequest
from retrapi.models.api.response import ClosestResponse


def create_closest_query(point: data_models.Point) -> sql.ClauseElement:
    return select(func.closest(point.x, point.y).table_valued("x", "y"))


def parse_closest_result(record: Mapping) -> database_models.Point:
    return database_models.Point.parse_obj(record)


async def query_closest(point: data_models.Point) -> database_models.Point:
    query = create_closest_query(point)
    result = await database.fetch_one(query)
    return parse_closest_result(result)


def parse_closest_point(point: database_models.Point) -> data_models.Point:
    return data_models.Point(x=point.x, y=point.y)


async def handle(request: ClosestRequest) -> ClosestResponse:
    db_point = await query_closest(request.point)
    data_point = parse_closest_point(db_point)
    return ClosestResponse(point=data_point)
