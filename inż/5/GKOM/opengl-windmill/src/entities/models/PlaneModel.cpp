#include "PlaneModel.h"

std::vector<Vertex> PlaneModel::calculateVertices(float width, float height)
{
    //generate vertices for ploane
    std::vector<Vertex> vertices = {
        Vertex(glm::vec3(-0.5f * width, -0.5f * height, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f), glm::vec2(0.0f, 0.0f)),
        Vertex(glm::vec3(0.5f * width, 0.5f * height, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f), glm::vec2(1.0f, 1.0f)),
        Vertex(glm::vec3(0.5f * width, -0.5f * height, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f), glm::vec2(1.0f, 0.0f)),
        Vertex(glm::vec3(0.5f * width, 0.5f * height, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f), glm::vec2(1.0f, 1.0f)),
        Vertex(glm::vec3(-0.5f * width, -0.5f * height, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f), glm::vec2(0.0f, 0.0f)),
        Vertex(glm::vec3(-0.5f * width, 0.5f * height, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f), glm::vec2(0.0f, 1.0f)),
    };

    return vertices;
}

PlaneModel::PlaneModel(float width, float height, int positionLocation, int normalLocation, int textureLocation) : BaseObjectModel(calculateVertices(width, height), positionLocation, normalLocation, textureLocation)
{
}