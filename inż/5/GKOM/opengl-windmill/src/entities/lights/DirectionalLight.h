#ifndef WIATRAK_DIRECTIONALLIGHT_H
#define WIATRAK_DIRECTIONALLIGHT_H


#include <GL/glew.h>
#include <glm/glm.hpp>
#include <glm/gtx/transform.hpp>
#include "../../utils/DirectionalLightAttributes.h"

class DirectionalLight
{
    const float MIN_INTENSITY = 0.0f, MAX_INTENSITY = 5.0f;

    DirectionalLightAttributes attributes;

    //depth map
    const unsigned int DEPTH_TEXTURE_UNIT = 1;
    const unsigned int SHADOW_WIDTH = 4096, SHADOW_HEIGHT = 4096;
    unsigned int depthFBO;
    unsigned int depthMap;
    ColorFloat borderColor = {1.0f, 1.0f, 1.0f};

    const float PLANE_SIZE = 20.0f, NEAR_PLANE = 1.0f, FAR_PLANE = 50.0f;
    const float LIGHT_DISTANCE = 3.0f;
    glm::mat4 lightSpaceMatrix;

    void initDepthMap();

    glm::mat4 getLightViewMatrix() const;

    glm::mat4 getLightProjectionMatrix() const;

public:
    DirectionalLight(DirectionalLightAttributes attributes);

    const DirectionalLightAttributes &getAttributes() const
    { return attributes; }

    unsigned int getDepthTextureUnit() const
    { return DEPTH_TEXTURE_UNIT; }

    unsigned int getDepthFBO() const
    { return depthFBO; }

    unsigned int getDepthMap() const
    { return depthMap; }

    unsigned int getShadowWidth() const
    { return SHADOW_WIDTH; }

    unsigned int getShadowHeight() const
    { return SHADOW_HEIGHT; }

    const glm::mat4 &getLightSpaceMatrix() const
    { return lightSpaceMatrix; }

    void setIntensity(float intensity)
    {
        intensity = std::max(MIN_INTENSITY, std::min(intensity, MAX_INTENSITY));

        attributes.globalIntensity = intensity;
    }
};


#endif //WIATRAK_DIRECTIONALLIGHT_H
