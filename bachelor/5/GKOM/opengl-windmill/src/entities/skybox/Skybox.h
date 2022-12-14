#ifndef WIATRAK_SKYBOX_H
#define WIATRAK_SKYBOX_H

#include "../Entity.h"
#include "../../utils/Material.h"
#include "../../utils/Texture.h"

class Skybox : public Entity
{
    Material material;
    GLuint textureID;

public:
    Skybox(const BaseObjectModel &model, Material material, Texture texture) : Entity(model), material(material), textureID(texture.getTextureID())
    {
    }

    const Material &getMaterial() const
    {
        return material;
    }

    const GLuint &getTextureID() const
    {
        return textureID;
    }
};

#endif //WIATRAK_SKYBOX_H