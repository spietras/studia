#include <utility>

#include <utility>
#include <iostream>

#include "BaseObjectModel.h"

BaseObjectModel::BaseObjectModel(const std::vector<glm::vec3> &points,
                                 int positionLocation) : points(points),
                                                         VAO(0), VBO(0)
{
    //generate VAO, VBO for each model
    glGenVertexArrays(1, &this->VAO);
    glGenBuffers(1, &this->VBO);

    glBindVertexArray(this->VAO); //bind VAO as current
    glBindBuffer(GL_ARRAY_BUFFER, this->VBO); //bind VBO to current VAO
    glBufferData(GL_ARRAY_BUFFER, getPointsByteSize(), points.data(), GL_STATIC_DRAW); //copy vertices data to VBO

    glVertexAttribPointer(positionLocation, 3, GL_FLOAT, GL_FALSE, sizeof(glm::vec3), nullptr); //set vertices atributes: 3 floats (x, y, z)
    glEnableVertexAttribArray(positionLocation);

    // unbind
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

BaseObjectModel::~BaseObjectModel()
{
    glDeleteVertexArrays(1, &(this->VAO));
    glDeleteBuffers(1, &(this->VBO));
}
