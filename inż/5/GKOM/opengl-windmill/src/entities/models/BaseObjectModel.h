#ifndef WIATRAK_BASEOBJECTMODEL_H
#define WIATRAK_BASEOBJECTMODEL_H

#include <vector>
#include "GL/glew.h"
#include "glm/vec3.hpp"
#include "../../utils/Vertex.h"

class BaseObjectModel
{
    std::vector<Vertex> vertices;

    unsigned int VAO;
    unsigned int VBO;

public:
    BaseObjectModel(std::vector<Vertex> vertices, int positionLocation, int normalLocation);

    ~BaseObjectModel();

    unsigned int getVAO() const
    { return VAO; }

    unsigned int getVBO() const
    { return VBO; }

    unsigned int getVerticesSize() const
    { return vertices.size(); }

    unsigned int getVerticesByteSize() const
    { return sizeof(Vertex) * vertices.size(); }
};


#endif //WIATRAK_BASEOBJECTMODEL_H
