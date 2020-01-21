#ifndef WIATRAK_CIRCULARFRUSTUM_H
#define WIATRAK_CIRCULARFRUSTUM_H


#include "BaseObjectModel.h"
#include "../../utils/GeometryHelper.h"

class CircularFrustum : public BaseObjectModel
{
    static std::vector<Vertex> calculateVertices(float downRadius,
                                                 float upRadius,
                                                 float height,
                                                 int baseVertices,
                                                 bool scaleTextures)
    {
        std::vector<glm::vec3> vertexPositons;

        for (int i = 0; i < baseVertices; ++i)
        {
            float part = i / (float) baseVertices;

            //Where the cylinder is in the x and z positions (3D space)

            glm::vec3 positionDown = {
                    downRadius * cos(2 * M_PI * part),
                    -0.5f * height,
                    downRadius * sin(2 * M_PI * part)
            };

            glm::vec3 positionUp = {
                    upRadius * cos(2 * M_PI * part),
                    0.5f * height,
                    upRadius * sin(2 * M_PI * part)
            };


            vertexPositons.push_back(positionDown);
            vertexPositons.push_back(positionUp);
        }

        std::vector<Vertex> vertices;

        for (int i = 0; i < baseVertices; ++i)
        {
            int index1 = 2 * i;
            int index2 = 2 * ((i + 1) % baseVertices);

            glm::vec3 sideNormal = GeometryHelper::getSurfaceNormal(vertexPositons[index1],
                                                                    vertexPositons[index1 + 1],
                                                                    vertexPositons[index2]);

            float texPart1 = i / (float) baseVertices;
            float texPart2 = (i + 1) / (float) baseVertices;

            vertices.emplace_back(vertexPositons[index1],                       sideNormal,     glm::vec2(texPart1,     0.0f    ));
            vertices.emplace_back(vertexPositons[index2 + 1],                   sideNormal,     glm::vec2(texPart2,     1.0f    ));
            vertices.emplace_back(vertexPositons[index2],                       sideNormal,     glm::vec2(texPart2,     0.0f    ));
            vertices.emplace_back(vertexPositons[index2 + 1],                   sideNormal,     glm::vec2(texPart2,     1.0f    ));
            vertices.emplace_back(vertexPositons[index1],                       sideNormal,     glm::vec2(texPart1,     0.0f    ));
            vertices.emplace_back(vertexPositons[index1 + 1],                   sideNormal,     glm::vec2(texPart1,     1.0f    ));

            glm::vec3 baseNormal = GeometryHelper::getSurfaceNormal(vertexPositons[index1 + 1],
                                                                    glm::vec3(0.0f, 0.5f * height, 0.0f),
                                                                    vertexPositons[index2 + 1]);

            vertices.emplace_back(vertexPositons[index1 + 1],                   baseNormal,     glm::vec2(texPart1,     0.0f    ));
            vertices.emplace_back(glm::vec3(0.0f, 0.5f * height, 0.0f),         baseNormal,     glm::vec2(texPart2,     1.0f    ));
            vertices.emplace_back(vertexPositons[index2 + 1],                   baseNormal,     glm::vec2(texPart2,     0.0f    ));

            vertices.emplace_back(vertexPositons[index1],                       -baseNormal,    glm::vec2(texPart1,     0.0f    ));
            vertices.emplace_back(glm::vec3(0.0f, -0.5f * height, 0.0f),        -baseNormal,    glm::vec2(texPart2,     1.0f    ));
            vertices.emplace_back(vertexPositons[index2],                       -baseNormal,    glm::vec2(texPart2,     0.0f    ));
        }

        return vertices;
    }

public:
    CircularFrustum(float downRadius,
                    float upRadius,
                    float height,
                    int baseVertices,
                    int positionLocation,
                    int normalLocation,
                    int textureLocation,
                    bool scaleTextures = true) : BaseObjectModel(calculateVertices(downRadius,
                                                                                   upRadius,
                                                                                   height,
                                                                                   baseVertices,
                                                                                   scaleTextures),
                                                                 positionLocation,
                                                                 normalLocation,
                                                                 textureLocation)
    {}
};

#endif //WIATRAK_CIRCULARFRUSTUM_H
