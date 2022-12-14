#include <utility>

#include <iostream>

#ifndef WIATRAK_COMPOSITEENTITY_H
#define WIATRAK_COMPOSITEENTITY_H


#include "Entity.h"
#include "absorbers/Absorber.h"
#include "lights/PointLight.h"

class CompositeEntity : public Entity
{
    const std::vector<Absorber *> compositeAbsorbers;
    const std::vector<PointLight *> compositeLights;

public:
    CompositeEntity(std::vector<Absorber *>  absorbers,
                    std::vector<PointLight *>  lights) : Entity(nullptr),
                                                        compositeAbsorbers(std::move(absorbers)),
                                                        compositeLights(std::move(lights))
    {
        for(Absorber* a : compositeAbsorbers)
            this->addChild(a);
        for(PointLight* l : compositeLights)
            this->addChild(l);
    }

    ~CompositeEntity()
    {
        for(Absorber* a : compositeAbsorbers)
            delete a;
        for(PointLight* l : compositeLights)
            delete l;
    }

    const std::vector<Absorber *> &getCompositeAbsorbers() const { return compositeAbsorbers; }

    const std::vector<PointLight *> &getCompositeLights() const { return compositeLights; }
};


#endif //WIATRAK_COMPOSITEENTITY_H
