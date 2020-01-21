#ifndef WIATRAK_VERTEX_H
#define WIATRAK_VERTEX_H

#include <vector>
#include <glm/vec3.hpp>
#include <glm/vec2.hpp>

class Vertex
{
    glm::vec3 positon; // position of vertex in model space
    glm::vec3 normal; // normal of vertex in model space
    glm::vec2 texture;

public:
    Vertex(glm::vec3 positon, glm::vec3 normal, glm::vec2 texture) : positon(positon), normal(normal), texture(texture)
    {}

    const glm::vec3 &getPosition() const
    { return positon; }

    const glm::vec3 &getNormal() const
    { return normal; }

    const glm::vec2 &getTexture() const
    { return texture; }

    void setTexture(const glm::vec2 &tex)
    { this->texture = tex; }

    static unsigned int getPositionDim()
    { return 3; }

    static unsigned int getPositionSize()
    { return sizeof(glm::vec3); }

    static long getPositionOffset()
    { return 0; }

    static unsigned int getNormalDim()
    { return 3; }

    static unsigned int getNormalSize()
    { return sizeof(glm::vec3); }

    static long getNormalOffset()
    { return getPositionOffset() + getPositionSize(); }

    static unsigned int getTextureDim()
    { return 2; }

    static unsigned int getTextureSize()
    { return sizeof(glm::vec2); }

    static long getTextureOffset()
    { return getNormalOffset() + getNormalSize(); }
};

#endif //WIATRAK_VERTEX_H
