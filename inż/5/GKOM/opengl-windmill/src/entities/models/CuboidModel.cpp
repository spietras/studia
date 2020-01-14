#include "CuboidModel.h"


std::vector<Vertex> CuboidModel::calculateVertices(float width, float height, float depth)
{
    //generate vertices for cuboid
    std::vector<Vertex> vertices = {
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f)),

            Vertex(glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f)),

            Vertex(glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f)),

            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f)),

            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f)),

            Vertex(glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f)),
    };

    return vertices;
}

CuboidModel::CuboidModel(float width, float height, float depth, int positionLocation, int normalLocation) :
        BaseObjectModel(calculateVertices(width, height, depth), positionLocation, normalLocation)
{}