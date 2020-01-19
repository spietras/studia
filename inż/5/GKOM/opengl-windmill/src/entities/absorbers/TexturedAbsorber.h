#ifndef WIATRAK_TEXTUREDABSORBER_H
#define WIATRAK_TEXTUREDABSORBER_H

#include "../Entity.h"
#include "../../utils/Material.h"
#include "../../utils/Texture.h"

#include "Absorber.h"

class TexturedAbsorber : public Absorber
{
    Texture texture;

public:
    TexturedAbsorber(const BaseObjectModel &model, Material material, Texture texture) : Absorber(model, material), texture(texture)
    {
    }

    const Texture &getTexture() const
    {
        return texture;
    }
};

#endif //WIATRAK_TEXTUREDABSORBER_H
