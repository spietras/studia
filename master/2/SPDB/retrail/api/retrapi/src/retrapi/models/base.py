from pydantic import BaseModel as PydanticBaseModel
from pydantic.generics import GenericModel as PydanticGenericModel


class BaseModel(PydanticBaseModel):
    class Config:
        frozen = True


class GenericModel(PydanticGenericModel):
    class Config:
        frozen = True
