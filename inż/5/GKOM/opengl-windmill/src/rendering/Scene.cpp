#include "Scene.h"

const std::vector<const Absorber *> &Scene::getAbsorbers() const
{
    return this->absorbers;
}

void Scene::addAbsorber(const Absorber &absorber)
{
    this->absorbers.push_back(&absorber);
}

const std::vector<const PointLight *> &Scene::getLights() const
{
    return this->lights;
}

void Scene::addLight(const PointLight &light)
{
    if(this->lights.size() >= MAX_POINT_LIGHTS)
        return;

    this->lights.push_back(&light);
}

const DirectionalLight * Scene::getDirectionalLight() const
{
    return this->directionalLight;
}

void Scene::setDirectionLight(const DirectionalLight &light)
{
    this->directionalLight = &light;
}

bool Scene::isShadowsTurnedOn() const
{
    return this->shadows;
}

bool Scene::turnOnShadows()
{
    if(directionalLight == nullptr)
        return false;

    shadows = true;
}

bool Scene::turnOffShadows()
{
    shadows = false;
}
