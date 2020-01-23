#ifndef WIATRAK_DIRECTIONALLIGHTATTRIBUTES_H
#define WIATRAK_DIRECTIONALLIGHTATTRIBUTES_H


#include "Color.h"

struct DirectionalLightAttributes
{
    glm::vec3 direction;

    ColorFloat color; // light color

    // intensities of particular light components
    float ambientIntensity;
    float diffuseIntensity;
    float specularIntensity;

    float globalIntensity;

    DirectionalLightAttributes(const glm::vec3 &direction,
                               ColorFloat color,
                               float ambientIntensity,
                               float diffuseIntensity,
                               float specularIntensity,
                               float globalIntensity = 1.0f) : direction(direction),
                                                               color(color),
                                                               ambientIntensity(ambientIntensity),
                                                               diffuseIntensity(diffuseIntensity),
                                                               specularIntensity(specularIntensity),
                                                               globalIntensity(globalIntensity)
    {}
};


#endif //WIATRAK_DIRECTIONALLIGHTATTRIBUTES_H
