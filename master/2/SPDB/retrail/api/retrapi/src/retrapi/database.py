from databases import Database

from retrapi.config import database as config

database = Database(config.uri)


async def on_startup() -> None:
    await database.connect()


async def on_shutdown() -> None:
    await database.disconnect()
