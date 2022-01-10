from fastapi import FastAPI

from retrapi.models.api.request import ClosestRequest, FindRequest
from retrapi.models.api.response import ClosestResponse, FindResponse
from retrapi.models.data import Line, Path

app = FastAPI(title="retrapi", description="retrail api")


@app.get("/about", response_model=str)
async def about():
    return app.title


@app.post("/closest", response_model=ClosestResponse)
async def closest(request: ClosestRequest):
    return ClosestResponse(point=request.point)


@app.post("/find", response_model=FindResponse)
async def find(request: FindRequest):
    return FindResponse(
        path=Path(
            lines=[
                Line(start=p1, end=p2)
                for p1, p2 in zip(request.points, request.points[1:])
            ]
        )
    )
