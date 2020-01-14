#ifndef WIATRAK_BASEOBJECTMODEL_H
#define WIATRAK_BASEOBJECTMODEL_H

#include <vector>
#include "GL/glew.h"
#include "glm/vec3.hpp"

class BaseObjectModel
{
    std::vector<glm::vec3> points; //vertices

    unsigned int VAO;
    unsigned int VBO;

public:
    BaseObjectModel(const std::vector<glm::vec3> &points, int positionLocation);
    ~BaseObjectModel();

    unsigned int getVAO() const
    { return VAO; }

    unsigned int getVBO() const
    { return VBO; }

    unsigned int getPointsSize() const
    { return points.size(); }

    unsigned int getPointsByteSize() const
    { return sizeof(glm::vec3) * points.size(); }
};


#endif //WIATRAK_BASEOBJECTMODEL_H
