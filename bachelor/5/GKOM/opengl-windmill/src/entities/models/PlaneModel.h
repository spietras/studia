#ifndef WIATRAK_PLANEMODEL_H
#define WIATRAK_PLANEMODEL_H

#include "BaseObjectModel.h"

class PlaneModel : public BaseObjectModel
{
    static std::vector<Vertex> calculateVertices(float width, float depth, bool scaleTextures)
    {
        float scaleX = scaleTextures ? 1.0f : width;
        float scaleZ = scaleTextures ? 1.0f : depth;

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

public:
    PlaneModel(float width,
               float depth,
               int positionLocation,
               int normalLocation,
               int textureLocation,
               bool scaleTextures = false)
            : BaseObjectModel(calculateVertices(width, depth, scaleTextures), positionLocation, normalLocation, textureLocation)
    {
    }
};

#endif //WIATRAK_PLANEMODEL_H
