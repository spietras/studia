from retrapi import config


async def handle() -> str:
    return config.app.title
