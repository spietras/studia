from pathlib import Path
from typing import Iterable, List, Union

import aiofiles

from retrapi import resource
from retrapi.models import data
from retrapi.models.api.response import BorderResponse

BORDER_FILE = "border.poly"


def parse_poly(lines: Iterable[str]) -> data.Path:
    """Parse an Osmosis polygon filter file.

    https://wiki.openstreetmap.org/wiki/Osmosis/Polygon_Filter_File_Python_Parsing
    """
    in_ring = False
    coords = []

    for (index, line) in enumerate(lines):
        if index == 0:
            # first line is junk.
            continue

        elif index == 1:
            # second line is the first polygon ring.
            in_ring = True

        elif in_ring and line.strip() == "END":
            # we are at the end of a ring
            in_ring = False

        elif not in_ring and line.strip() == "END":
            # we are at the end of the whole polygon.
            break

        elif in_ring:
            # we are in a ring and picking up new coordinates.
            coords.append(tuple(map(float, line.split())))

        elif not in_ring:
            # we are at the start of a polygon part.
            in_ring = True
        else:
            raise ValueError("Wrong format of .poly file")

    return data.Path(
        lines=[
            data.Line(
                start=data.Point(x=x1, y=y1),
                end=data.Point(x=x2, y=y2),
            )
            for (x1, y1), (x2, y2) in zip(coords, coords[1:])
        ]
    )


async def read_lines(path: Union[str, Path]) -> List[str]:
    async with aiofiles.open(path) as file:
        return await file.readlines()


async def read_poly(path: Union[str, Path]) -> data.Path:
    lines = await read_lines(path)
    return parse_poly(lines)


async def handle() -> BorderResponse:
    with resource(BORDER_FILE) as path:
        border = await read_poly(path)
    return BorderResponse(border=border)
