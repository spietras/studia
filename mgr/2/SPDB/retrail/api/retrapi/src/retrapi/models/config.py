import os

from pydantic import BaseModel


class DatabaseConfig(BaseModel):
    user: str = os.getenv("RETRAPI_POSTGRES_USER", "app")
    password: str = os.getenv("RETRAPI_POSTGRES_PASSWORD", "app")
    host: str = os.getenv("RETRAPI_POSTGRES_HOST", "postgres")
    port: int = int(os.getenv("RETRAPI_POSTGRES_PORT", "5432"))
    database: str = os.getenv("RETRAPI_POSTGRES_DATABASE", "postgres")

    @property
    def uri(self) -> str:
        return f"postgresql+psycopg2://{self.user}:{self.password}@{self.host}:{self.port}/{self.database}"
