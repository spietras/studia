#ifndef WIATRAK_ABSORBER_H
#define WIATRAK_ABSORBER_H


#include "../Entity.h"

class Absorber : public Entity
{
    ColorFloat color;
public:
    Absorber(const BaseObjectModel &model, ColorFloat color) : Entity(model), color(color)
    {}

    const ColorFloat &getColor() const
    { return color; }
};


#endif //WIATRAK_ABSORBER_H
