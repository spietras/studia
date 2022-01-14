from retrapi.models.api.request import FindRequest
from retrapi.models.api.response import FindResponse
from retrapi.models.data import Line, Path


async def handle(request: FindRequest) -> FindResponse:
    return FindResponse(
        path=Path(
            lines=[
                Line(start=p1, end=p2)
                for p1, p2 in zip(request.points, request.points[1:])
            ]
        )
    )
