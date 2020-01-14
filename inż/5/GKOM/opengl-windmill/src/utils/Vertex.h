#ifndef WIATRAK_VERTEX_H
#define WIATRAK_VERTEX_H

#include <vector>
#include <glm/vec3.hpp>

class Vertex
{
    glm::vec3 positon;
    glm::vec3 normal;

public:
    Vertex(glm::vec3 positon, glm::vec3 normal) : positon(positon), normal(normal) {}

    const glm::vec3& getPosition() const { return positon; }
    const glm::vec3& getNormal() const { return normal; }

    static unsigned int getPositionDim() { return 3; }
    static unsigned int getPositionSize() { return sizeof(glm::vec3); }
    static long getPositionOffset() { return 0; }
    static unsigned int getNormalDim() { return 3; }
    static unsigned int getNormalSize() { return sizeof(glm::vec3); }
    static long getNormalOffset() { return getPositionOffset() + getPositionSize(); }
};

#endif //WIATRAK_VERTEX_H
