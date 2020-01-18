#ifndef WIATRAK_SKYBOX_H
#define WIATRAK_SKYBOX_H

#include "../Entity.h"
#include "../../utils/Material.h"

class Skybox : public Entity
{
    Material material;
    GLuint textureID;

public:
    Skybox(const BaseObjectModel &model, Material material, GLuint textureID) : Entity(model), material(material), textureID(textureID)
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