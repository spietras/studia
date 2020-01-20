#ifndef WIATRAK_ABSORBER_H
#define WIATRAK_ABSORBER_H

#include "../Entity.h"
#include "../../utils/Material.h"
#include "../../utils/Texture.h"

class Absorber : public Entity
{
    Material material;
    Texture texture;
    int mode; // 0 only material, 1 texture
public:
    static const int COLOR = 0;
    static const int TEXTURE = 1;

    Absorber(const BaseObjectModel &model, Material material) : Entity(model), material(material), mode(COLOR)
    {
    }

    Absorber(const BaseObjectModel &model, Material material, Texture texture) : Entity(model), material(material), texture(texture), mode(TEXTURE)
    {
    }

    const Material &getMaterial() const
    {
        return material;
    }

    const Texture &getTexture() const
    {
        return texture;
    }

    const int getMode() const
    {
        return mode;
    }
};

#endif //WIATRAK_ABSORBER_H
