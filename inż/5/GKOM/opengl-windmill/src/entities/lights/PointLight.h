#ifndef WIATRAK_POINTLIGHT_H
#define WIATRAK_POINTLIGHT_H


#include "../Entity.h"
#include "../../utils/PointLightAttributes.h"

class PointLight : public Entity
{
    const float MIN_INTENSITY = 0.0f, MAX_INTENSITY = 5.0f;

    PointLightAttributes attributes;
public:
    PointLight(const BaseObjectModel &model, PointLightAttributes attributes) : Entity(model), attributes(attributes)
    {}

    const PointLightAttributes &getAttributes() const
    { return attributes; }

    void setIntensity(float intensity)
    {
        intensity = std::max(MIN_INTENSITY, std::min(intensity, MAX_INTENSITY));

        attributes.globalIntensity = intensity;
    }
};


#endif //WIATRAK_POINTLIGHT_H
