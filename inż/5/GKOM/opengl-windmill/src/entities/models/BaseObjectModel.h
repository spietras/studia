#ifndef WIATRAK_BASEOBJECTMODEL_H
#define WIATRAK_BASEOBJECTMODEL_H

#include <vector>
#include "GL/glew.h"
#include "glm/vec3.hpp"
#include "../../utils/Triangle.h"

class BaseObjectModel
{
    std::vector<glm::vec3> points; //vertices
    std::vector<Triangle> triangles; //indices

    unsigned int VAO;
    unsigned int VBO;
    unsigned int EBO;

public:
    BaseObjectModel(const std::vector<glm::vec3> &points, const std::vector<Triangle> &triangles, int positionLocation);
    ~BaseObjectModel();

    unsigned int getVAO() const
    { return VAO; }

    unsigned int getVBO() const
    { return VBO; }

    unsigned int getEBO() const
    { return EBO; }

    unsigned int getPointsByteSize() const
    { return sizeof(glm::vec3) * points.size(); }

    unsigned int getTrianglesByteSize() const
    { return sizeof(Triangle) * triangles.size(); }
};


#endif //WIATRAK_BASEOBJECTMODEL_H
