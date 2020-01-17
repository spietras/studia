#ifndef WIATRAK_POINTLIGHT_H
#define WIATRAK_POINTLIGHT_H


#include "../Entity.h"
#include "../../utils/PointLightAttributes.h"

class PointLight : public Entity
{
    PointLightAttributes attributes;
public:
    PointLight(const BaseObjectModel &model, PointLightAttributes attributes) : Entity(model), attributes(attributes)
    {}

    const PointLightAttributes &getAttributes() const
    { return attributes; }
};


#endif //WIATRAK_POINTLIGHT_H
