from fastapi import FastAPI

from retrapi import config, database
from retrapi.handlers.about import handle as handle_about
from retrapi.handlers.border import handle as handle_border
from retrapi.handlers.closest import handle as handle_closest
from retrapi.handlers.find import handle as handle_find
from retrapi.models.api.request import ClosestRequest, FindRequest
from retrapi.models.api.response import (
    BorderResponse,
    ClosestResponse,
    FindResponse,
)

app = FastAPI(title=config.app.title, description=config.app.description)


@app.on_event("startup")
async def startup():
    await database.on_startup()


@app.on_event("shutdown")
async def shutdown():
    await database.on_shutdown()


@app.get("/about", response_model=str)
async def about():
    return await handle_about()


@app.get("/border", response_model=BorderResponse)
async def border():
    return await handle_border()


@app.post("/closest", response_model=ClosestResponse)
async def closest(request: ClosestRequest):
    return await handle_closest(request)


@app.post("/find", response_model=FindResponse)
async def find(request: FindRequest):
    return await handle_find(request)
