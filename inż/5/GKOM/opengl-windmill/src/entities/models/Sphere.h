#ifndef WIATRAK_SPHERE_H
#define WIATRAK_SPHERE_H

#include "BaseObjectModel.h"
#include "../../utils/GeometryHelper.h"

class Sphere : public BaseObjectModel
{
    static std::vector<Vertex> calculateVertices(float radius,
                                                 int segmentsHorizontal,
                                                 int segmentsVertical,
                                                 bool scaleTextures)
    {
        float scale = 2.0f * M_PI * radius;

        std::vector<glm::vec3> positions;
        std::vector<glm::vec2> uvs;

        for (unsigned int y = 0; y <= segmentsVertical; ++y)
        {
            for (unsigned int x = 0; x <= segmentsHorizontal; ++x)
            {
                float xSegment = (float)x / (float)segmentsHorizontal;
                float ySegment = (float)y / (float)segmentsVertical;
                float xPos = radius * std::cos(xSegment * 2.0f * M_PI) * std::sin(ySegment * M_PI);
                float yPos = radius * std::cos(ySegment * M_PI);
                float zPos = radius * std::sin(xSegment * 2.0f * M_PI) * std::sin(ySegment * M_PI);

                positions.emplace_back(xPos, yPos, zPos);
                uvs.emplace_back(xSegment * scale, ySegment * scale);
            }
        }

        std::vector<Vertex> vertices;

        for (int y = 0; y < segmentsVertical; ++y)
        {
            for (int x = 0; x < segmentsHorizontal; ++x)
            {
                std::vector<int> ids = {
                        (y + 1) * (segmentsHorizontal + 1) + x,
                        y       * (segmentsHorizontal + 1) + x,
                        y       * (segmentsHorizontal + 1) + x + 1,

                        (y + 1) * (segmentsHorizontal + 1) + x,
                        y       * (segmentsHorizontal + 1) + x + 1,
                        (y + 1) * (segmentsHorizontal + 1) + x + 1
                };

                glm::vec3 normal = GeometryHelper::getSurfaceNormal(positions[ids[0]],
                                                                    positions[ids[1]],
                                                                    positions[ids[2]]);

                for(int id : ids)
                    vertices.emplace_back(Vertex(positions[id], normal, uvs[id]));
            }
        }

        return vertices;
    }

public:
    Sphere(float radius,
           int segmentsHorizontal,
           int segmentsVertical,
           int positionLocation,
           int normalLocation,
           int textureLocation,
           bool scaleTextures = true) : BaseObjectModel(calculateVertices(radius,
                                                                          segmentsHorizontal,
                                                                          segmentsVertical,
                                                                          scaleTextures),
                                                        positionLocation,
                                                        normalLocation,
                                                        textureLocation)
    {}
};


#endif //WIATRAK_SPHERE_H
