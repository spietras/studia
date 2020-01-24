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
    if (this->lights.size() >= MAX_POINT_LIGHTS)
        return;

    this->lights.push_back(&light);
}

const DirectionalLight * Scene::getDirectionalLight() const
{
    return this->directionalLight;
}

void Scene::setDirectionalLight(const DirectionalLight &light)
{
    this->directionalLight = &light;
}

void Scene::addCompositeEntity(const CompositeEntity &compositeEntity)
{
    for(Absorber* a : compositeEntity.getCompositeAbsorbers())
        this->addAbsorber(*a);
    for(PointLight* l : compositeEntity.getCompositeLights())
        this->addLight(*l);
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
    return true;
}

bool Scene::turnOffShadows()
{
    shadows = false;
    return true;
}


const std::vector<const Skybox *> &Scene::getSkybox() const
{
    return this->skybox;
}

void Scene::addSkybox(const Skybox &skybox)
{
    this->skybox.push_back(&skybox);
}