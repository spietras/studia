#ifndef WIATRAK_DIRECTIONALLIGHT_H
#define WIATRAK_DIRECTIONALLIGHT_H


#include "../../utils/DirectionalLightAttributes.h"

class DirectionalLight
{
    DirectionalLightAttributes attributes;
public:
    DirectionalLight(DirectionalLightAttributes attributes) : attributes(attributes) {}

    const DirectionalLightAttributes &getAttributes() const
    { return attributes; }
};


#endif //WIATRAK_DIRECTIONALLIGHT_H
