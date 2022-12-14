#ifndef WIATRAK_GEOMETRYHELPER_H
#define WIATRAK_GEOMETRYHELPER_H


#include <glm/glm.hpp>

class GeometryHelper
{
public:
    static glm::vec3 getSurfaceNormal(const glm::vec3 &pointA,
                                      const glm::vec3 &pointB,
                                      const glm::vec3 &pointC)
    {
        return glm::normalize(glm::cross(pointA - pointB, pointB - pointC));
    }
};


#endif //WIATRAK_GEOMETRYHELPER_H
