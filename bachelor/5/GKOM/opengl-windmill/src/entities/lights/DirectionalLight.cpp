#include "DirectionalLight.h"

void DirectionalLight::initDepthMap()
{
    //generate framebuffer for depth map
    glGenFramebuffers(1, &this->depthFBO);

    //generate texture to store depth into
    glGenTextures(1, &this->depthMap);
    glActiveTexture(GL_TEXTURE0 + DEPTH_TEXTURE_UNIT);
    glBindTexture(GL_TEXTURE_2D, this->depthMap);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, SHADOW_WIDTH, SHADOW_HEIGHT, 0, GL_DEPTH_COMPONENT, GL_FLOAT,
                 NULL);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
    float border[] = {this->borderColor.red, this->borderColor.green, this->borderColor.blue, this->borderColor.alpha};
    glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, border);

    //bind texture to framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, this->depthFBO);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, this->depthMap, 0);
    glDrawBuffer(GL_NONE);
    glReadBuffer(GL_NONE);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

glm::mat4 DirectionalLight::getLightViewMatrix() const
{
    glm::vec3 position = -LIGHT_DISTANCE * attributes.direction;
    glm::vec3 target = {0.0f, 0.0f, 0.0f}; //look at origin
    glm::vec3 up = {0.0f, 1.0f, 0.0f};

    // if look direction and up are parallel, we have to change up to anything else
    glm::vec3 lookDir = target - position;
    if(lookDir.x == 0.0f && lookDir.z == 0.0f)
        up = {0.0f, 0.0f, 1.0f};

    return glm::lookAt(position, target, up);
}

glm::mat4 DirectionalLight::getLightProjectionMatrix() const
{
    return glm::ortho(-0.5f * PLANE_SIZE, 0.5f * PLANE_SIZE, -0.5f * PLANE_SIZE, 0.5f * PLANE_SIZE, NEAR_PLANE,
                      FAR_PLANE);
}


DirectionalLight::DirectionalLight(DirectionalLightAttributes attributes) : attributes(attributes), depthFBO(0),
                                                                            depthMap(0)
{
    initDepthMap();

    glm::mat4 lightProjection = getLightProjectionMatrix();
    glm::mat4 lightView = getLightViewMatrix();

    this->lightSpaceMatrix = lightProjection * lightView;
}