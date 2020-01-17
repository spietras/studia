#include "Scene.h"

const std::vector<const Entity*> &Scene::getEntities() const
{
    return this->entities;
}

void Scene::addEntity(const Entity &entity)
{
    this->entities.push_back(&entity);
}