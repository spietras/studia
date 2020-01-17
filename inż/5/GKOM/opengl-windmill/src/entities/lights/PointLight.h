#ifndef WIATRAK_POINTLIGHT_H
#define WIATRAK_POINTLIGHT_H


#include "../Entity.h"

class PointLight : public Entity
{
    ColorFloat lightColor;
public:
    PointLight(const BaseObjectModel &model, ColorFloat lightColor) : Entity(model), lightColor(lightColor)
    {}

    const ColorFloat &getColor() const
    { return lightColor; }
};


#endif //WIATRAK_POINTLIGHT_H
