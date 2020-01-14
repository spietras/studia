#include "CuboidModel.h"


std::vector<glm::vec3> CuboidModel::calculatePoints(float width, float height, float depth)
{
    //generate vertices for cuboid
    std::vector<glm::vec3> positions = {
            glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth),
            glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth),
            glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth),

            glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth),
            glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth),
            glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth),
            glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth),
            glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth),
            glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth),

            glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth),
            glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth),
            glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth),
            glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth),
            glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth),
            glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth),

            glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth),
            glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth),
            glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth),

            glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width, -0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth),
            glm::vec3( 0.5f * width, -0.5f * height,  0.5f * depth),
            glm::vec3(-0.5f * width, -0.5f * height,  0.5f * depth),
            glm::vec3(-0.5f * width, -0.5f * height, -0.5f * depth),

            glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth),
            glm::vec3( 0.5f * width,  0.5f * height, -0.5f * depth),
            glm::vec3( 0.5f * width,  0.5f * height,  0.5f * depth),
            glm::vec3(-0.5f * width,  0.5f * height, -0.5f * depth),
            glm::vec3(-0.5f * width,  0.5f * height,  0.5f * depth),
    };

    return positions;
}

CuboidModel::CuboidModel(float width, float height, float depth, int positionLocation) :
        BaseObjectModel(calculatePoints(width, height, depth), positionLocation)
{}