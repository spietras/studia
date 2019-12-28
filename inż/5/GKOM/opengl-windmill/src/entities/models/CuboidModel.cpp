#include "CuboidModel.h"


std::vector<glm::vec3> CuboidModel::calculatePoints(float width, float height, float depth)
{
    //generate vertices for cuboid
    std::vector<glm::vec3> points;
    points.emplace_back(0.0f - width * 0.5f, 0.0f - height * 0.5f, 0.0f - depth * 0.5f);
    points.emplace_back(width - width * 0.5f, 0.0f - height * 0.5f, 0.0f - depth * 0.5f);
    points.emplace_back(0.0f - width * 0.5f, 0.0f - height * 0.5f, depth - depth * 0.5f);
    points.emplace_back(width - width * 0.5f, 0.0f - height * 0.5f, depth - depth * 0.5f);
    points.emplace_back(0.0f - width * 0.5f, height - height * 0.5f, 0.0f - depth * 0.5f);
    points.emplace_back(width - width * 0.5f, height - height * 0.5f, 0.0f - depth * 0.5f);
    points.emplace_back(0.0f - width * 0.5f, height - height * 0.5f, depth - depth * 0.5f);
    points.emplace_back(width - width * 0.5f, height - height * 0.5f, depth - depth * 0.5f);
    return points;
}

std::vector<Triangle> CuboidModel::calculateTriangles()
{
    // generate indices for cuboid
    std::vector<Triangle> triangles;
    triangles.emplace_back(0, 4, 1);
    triangles.emplace_back(4, 1, 5);
    triangles.emplace_back(2, 6, 3);
    triangles.emplace_back(6, 3, 7);
    triangles.emplace_back(4, 6, 5);
    triangles.emplace_back(6, 5, 7);
    triangles.emplace_back(0, 2, 1);
    triangles.emplace_back(2, 1, 3);
    triangles.emplace_back(1, 5, 3);
    triangles.emplace_back(5, 3, 7);
    triangles.emplace_back(0, 4, 2);
    triangles.emplace_back(4, 2, 6);
    return triangles;
}


CuboidModel::CuboidModel(float width, float height, float depth, int positionLocation) :
        BaseObjectModel(calculatePoints(width, height, depth), calculateTriangles(), positionLocation)
{}