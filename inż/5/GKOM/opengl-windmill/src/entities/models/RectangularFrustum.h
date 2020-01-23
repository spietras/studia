#ifndef WIATRAK_RECTANGULARFRUSTUM_H
#define WIATRAK_RECTANGULARFRUSTUM_H


#include "BaseObjectModel.h"
#include "../../utils/GeometryHelper.h"

class RectangularFrustum : public BaseObjectModel
{
    static std::vector<Vertex> calculateVertices(float downWidth,
                                                 float downDepth,
                                                 float upWidth,
                                                 float upDepth,
                                                 float height,
                                                 bool scaleTextures)
    {
        float scaleDownX = scaleTextures    ? 1.0f : downWidth;
        float scaleUpX = scaleTextures      ? 1.0f : upWidth;
        float scaleY = scaleTextures        ? 1.0f : height;
        float scaleDownZ = scaleTextures    ? 1.0f : downDepth;
        float scaleUpZ = scaleTextures      ? 1.0f : upDepth;

        glm::vec3 surf1Normal = GeometryHelper::getSurfaceNormal(glm::vec3(-0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),
                                                                 glm::vec3( 0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),
                                                                 glm::vec3( 0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ));
        glm::vec3 surf2Normal = GeometryHelper::getSurfaceNormal(glm::vec3(-0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),
                                                                 glm::vec3( 0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),
                                                                 glm::vec3( 0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ));
        glm::vec3 surf3Normal = GeometryHelper::getSurfaceNormal(glm::vec3(-0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),
                                                                 glm::vec3(-0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),
                                                                 glm::vec3(-0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ));
        glm::vec3 surf4Normal = GeometryHelper::getSurfaceNormal(glm::vec3( 0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),
                                                                 glm::vec3( 0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),
                                                                 glm::vec3( 0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ));
        glm::vec3 surf5Normal = GeometryHelper::getSurfaceNormal(glm::vec3(-0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),
                                                                 glm::vec3( 0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),
                                                                 glm::vec3( 0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ));
        glm::vec3 surf6Normal = GeometryHelper::getSurfaceNormal(glm::vec3(-0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),
                                                                 glm::vec3( 0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),
                                                                 glm::vec3( 0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ));

        //generate vertices for cuboid
        std::vector<Vertex> vertices = {
                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf1Normal, glm::vec2(0.0f * scaleDownX,     0.0f * scaleY)),
                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),         surf1Normal, glm::vec2(1.0f * scaleUpX,       1.0f * scaleY)),
                Vertex(glm::vec3( 0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf1Normal, glm::vec2(1.0f * scaleDownX,     0.0f * scaleY)),
                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),         surf1Normal, glm::vec2(1.0f * scaleUpX,       1.0f * scaleY)),
                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf1Normal, glm::vec2(0.0f * scaleDownX,     0.0f * scaleY)),
                Vertex(glm::vec3(-0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),         surf1Normal, glm::vec2(0.0f * scaleUpX,       1.0f * scaleY)),

                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),         surf2Normal, glm::vec2(0.0f * scaleDownX,     0.0f * scaleY)),
                Vertex(glm::vec3( 0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),         surf2Normal, glm::vec2(1.0f * scaleDownX,     0.0f * scaleY)),
                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf2Normal, glm::vec2(1.0f * scaleUpX,       1.0f * scaleY)),
                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf2Normal, glm::vec2(1.0f * scaleUpX,       1.0f * scaleY)),
                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),         surf2Normal, glm::vec2(0.0f * scaleDownX,     0.0f * scaleY)),
                Vertex(glm::vec3(-0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf2Normal, glm::vec2(0.0f * scaleUpX,       1.0f * scaleY)),

                Vertex(glm::vec3(-0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf3Normal, glm::vec2(1.0f * scaleY,         0.0f * scaleUpZ)),
                Vertex(glm::vec3(-0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),         surf3Normal, glm::vec2(1.0f * scaleY,         1.0f * scaleUpZ)),
                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf3Normal, glm::vec2(0.0f * scaleY,         1.0f * scaleDownZ)),
                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf3Normal, glm::vec2(0.0f * scaleY,         1.0f * scaleDownZ)),
                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),         surf3Normal, glm::vec2(0.0f * scaleY,         0.0f * scaleDownZ)),
                Vertex(glm::vec3(-0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf3Normal, glm::vec2(1.0f * scaleY,         0.0f * scaleUpZ)),

                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf4Normal, glm::vec2(1.0f * scaleY,         0.0f * scaleUpZ)),
                Vertex(glm::vec3( 0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf4Normal, glm::vec2(0.0f * scaleY,         1.0f * scaleDownZ)),
                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),         surf4Normal, glm::vec2(1.0f * scaleY,         1.0f * scaleUpZ)),
                Vertex(glm::vec3( 0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf4Normal, glm::vec2(0.0f * scaleY,         1.0f * scaleDownZ)),
                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf4Normal, glm::vec2(1.0f * scaleY,         0.0f * scaleUpZ)),
                Vertex(glm::vec3( 0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),         surf4Normal, glm::vec2(0.0f * scaleY,         0.0f * scaleDownZ)),

                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf5Normal, glm::vec2(0.0f * scaleDownX,     1.0f * scaleDownZ)),
                Vertex(glm::vec3( 0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf5Normal, glm::vec2(1.0f * scaleDownX,     1.0f * scaleDownZ)),
                Vertex(glm::vec3( 0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),         surf5Normal, glm::vec2(1.0f * scaleDownX,     0.0f * scaleDownZ)),
                Vertex(glm::vec3( 0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),         surf5Normal, glm::vec2(1.0f * scaleDownX,     0.0f * scaleDownZ)),
                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height,  0.5f * downDepth   ),         surf5Normal, glm::vec2(0.0f * scaleDownX,     0.0f * scaleDownZ)),
                Vertex(glm::vec3(-0.5f * downWidth,     -0.5f * height, -0.5f * downDepth   ),         surf5Normal, glm::vec2(0.0f * scaleDownX,     1.0f * scaleDownZ)),

                Vertex(glm::vec3(-0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),         surf6Normal, glm::vec2(0.0f * scaleUpX,       1.0f * scaleUpZ)),
                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf6Normal, glm::vec2(1.0f * scaleUpX,       0.0f * scaleUpZ)),
                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),         surf6Normal, glm::vec2(0.0f * scaleUpX,       1.0f * scaleUpZ)),
                Vertex(glm::vec3( 0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf6Normal, glm::vec2(1.0f * scaleUpX,       0.0f * scaleUpZ)),
                Vertex(glm::vec3(-0.5f * upWidth,        0.5f * height, -0.5f * upDepth     ),         surf6Normal, glm::vec2(0.0f * scaleUpX,       1.0f * scaleUpZ)),
                Vertex(glm::vec3(-0.5f * upWidth,        0.5f * height,  0.5f * upDepth     ),         surf6Normal, glm::vec2(0.0f * scaleUpX,       0.0f * scaleUpZ)),
        };

        return vertices;
    }

public:
    RectangularFrustum(float downWidth,
                     float downDepth,
                     float upWidth,
                     float upDepth,
                     float height,
                     int positionLocation,
                     int normalLocation,
                     int textureLocation,
                     bool scaleTextures = true) : BaseObjectModel(calculateVertices(downWidth,
                                                                                    downDepth,
                                                                                    upWidth,
                                                                                    upDepth,
                                                                                    height,
                                                                                    scaleTextures),
                                                                  positionLocation,
                                                                  normalLocation,
                                                                  textureLocation)
    {}
};

#endif //WIATRAK_RECTANGULARFRUSTUM_H
