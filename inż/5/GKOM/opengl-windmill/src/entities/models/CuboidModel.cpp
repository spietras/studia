#include "CuboidModel.h"


std::vector<Vertex> CuboidModel::calculateVertices(float width, float height, float depth, bool scaleTextures)
{
    float scaleX = scaleTextures ? width : 1.0f;
    float scaleY = scaleTextures ? height : 1.0f;
    float scaleZ = scaleTextures ? depth : 1.0f;

    //generate vertices for cuboid
    std::vector<Vertex> vertices = {
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f), glm::vec2(0.0f * scaleX, 0.0f * scaleY)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f), glm::vec2(1.0f * scaleX, 1.0f * scaleY)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f), glm::vec2(1.0f * scaleX, 0.0f * scaleY)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f), glm::vec2(1.0f * scaleX, 1.0f * scaleY)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f), glm::vec2(0.0f * scaleX, 0.0f * scaleY)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  0.0f, -1.0f), glm::vec2(0.0f * scaleX, 1.0f * scaleY)),

            Vertex(glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f), glm::vec2(0.0f * scaleX, 0.0f * scaleY)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f), glm::vec2(1.0f * scaleX, 0.0f * scaleY)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f), glm::vec2(1.0f * scaleX, 1.0f * scaleY)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f), glm::vec2(1.0f * scaleX, 1.0f * scaleY)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f), glm::vec2(0.0f * scaleX, 1.0f * scaleY)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  0.0f,  1.0f), glm::vec2(0.0f * scaleX, 0.0f * scaleY)),

            Vertex(glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f), glm::vec2(1.0f * scaleY, 0.0f * scaleZ)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f), glm::vec2(1.0f * scaleY, 1.0f * scaleZ)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f), glm::vec2(0.0f * scaleY, 1.0f * scaleZ)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f), glm::vec2(0.0f * scaleY, 1.0f * scaleZ)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f), glm::vec2(0.0f * scaleY, 0.0f * scaleZ)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3(-1.0f,  0.0f,  0.0f), glm::vec2(1.0f * scaleY, 0.0f * scaleZ)),

            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f), glm::vec2(1.0f * scaleY, 0.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f), glm::vec2(0.0f * scaleY, 1.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f), glm::vec2(1.0f * scaleY, 1.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f), glm::vec2(0.0f * scaleY, 1.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f), glm::vec2(1.0f * scaleY, 0.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 1.0f,  0.0f,  0.0f), glm::vec2(0.0f * scaleY, 0.0f * scaleZ)),

            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f), glm::vec2(0.0f * scaleX, 1.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f), glm::vec2(1.0f * scaleX, 1.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f), glm::vec2(1.0f * scaleX, 0.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f), glm::vec2(1.0f * scaleX, 0.0f * scaleZ)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f), glm::vec2(0.0f * scaleX, 0.0f * scaleZ)),
            Vertex(glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth), glm::vec3( 0.0f, -1.0f,  0.0f), glm::vec2(0.0f * scaleX, 1.0f * scaleZ)),

            Vertex(glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f), glm::vec2(0.0f * scaleX, 1.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f), glm::vec2(1.0f * scaleX, 0.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f), glm::vec2(0.0f * scaleX, 1.0f * scaleZ)),
            Vertex(glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f), glm::vec2(1.0f * scaleX, 0.0f * scaleZ)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f), glm::vec2(0.0f * scaleX, 1.0f * scaleZ)),
            Vertex(glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth), glm::vec3( 0.0f,  1.0f,  0.0f), glm::vec2(0.0f * scaleX, 0.0f * scaleZ)),
    };

    return vertices;
}

CuboidModel::CuboidModel(float width,
                         float height,
                         float depth,
                         int positionLocation,
                         int normalLocation,
                         int textureLocation,
                         bool scaleTextures) :
        BaseObjectModel(calculateVertices(width, height, depth, scaleTextures), positionLocation, normalLocation, textureLocation)
{}