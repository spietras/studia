#include "PlaneModel.h"

std::vector<Vertex> PlaneModel::calculateVertices(float width, float depth, bool scaleTextures)
{
    float scaleX = scaleTextures ? width : 1.0f;
    float scaleZ = scaleTextures ? depth : 1.0f;

    //generate vertices for plane
    std::vector<Vertex> vertices = {
        Vertex(glm::vec3(-0.5f * width, 0.0f, -0.5f * depth), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec2(0.0f * scaleX, 0.0f * scaleZ)),
        Vertex(glm::vec3( 0.5f * width, 0.0f,  0.5f * depth), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec2(1.0f * scaleX, 1.0f * scaleZ)),
        Vertex(glm::vec3( 0.5f * width, 0.0f, -0.5f * depth), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec2(1.0f * scaleX, 0.0f * scaleZ)),
        Vertex(glm::vec3( 0.5f * width, 0.0f,  0.5f * depth), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec2(1.0f * scaleX, 1.0f * scaleZ)),
        Vertex(glm::vec3(-0.5f * width, 0.0f, -0.5f * depth), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec2(0.0f * scaleX, 0.0f * scaleZ)),
        Vertex(glm::vec3(-0.5f * width, 0.0f,  0.5f * depth), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec2(0.0f * scaleX, 1.0f * scaleZ)),
    };

    return vertices;
}

PlaneModel::PlaneModel(float width,
                       float depth,
                       int positionLocation,
                       int normalLocation,
                       int textureLocation,
                       bool scaleTextures)
        : BaseObjectModel(calculateVertices(width, depth, scaleTextures), positionLocation, normalLocation, textureLocation)
{
}