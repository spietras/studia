#ifndef WIATRAK_ABSORBER_H
#define WIATRAK_ABSORBER_H


#include "../Entity.h"
#include "../../utils/Material.h"

class Absorber : public Entity
{
    Material material;
public:
    Absorber(const BaseObjectModel &model, Material material) : Entity(model), material(material)
    {}

    const Material &getMaterial() const
    { return material; }
};


#endif //WIATRAK_ABSORBER_H
