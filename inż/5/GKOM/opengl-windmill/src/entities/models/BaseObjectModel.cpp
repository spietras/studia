#include "BaseObjectModel.h"

BaseObjectModel::BaseObjectModel(std::vector<Vertex> vertices,
                                 int positionLocation, int normalLocation) : vertices(std::move(vertices)),
                                                                             VAO(0), VBO(0)
{
    //generate VAO, VBO for each model
    glGenVertexArrays(1, &this->VAO);
    glGenBuffers(1, &this->VBO);

    glBindVertexArray(this->VAO); //bind VAO as current
    glBindBuffer(GL_ARRAY_BUFFER, this->VBO); //bind VBO to current VAO
    glBufferData(GL_ARRAY_BUFFER, getVerticesByteSize(), this->vertices.data(),
                 GL_STATIC_DRAW); //copy vertices data to VBO

    glVertexAttribPointer(positionLocation, Vertex::getPositionDim(), GL_FLOAT, GL_FALSE, sizeof(Vertex),
                          (void *) Vertex::getPositionOffset()); //set position attribute
    glVertexAttribPointer(normalLocation, Vertex::getNormalDim(), GL_FLOAT, GL_FALSE, sizeof(Vertex),
                          (void *) Vertex::getNormalOffset()); //set normal attribute
    glEnableVertexAttribArray(positionLocation);
    glEnableVertexAttribArray(normalLocation);

    // unbind
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

BaseObjectModel::~BaseObjectModel()
{
    glDeleteVertexArrays(1, &(this->VAO));
    glDeleteBuffers(1, &(this->VBO));
}
