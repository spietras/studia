#ifndef WIATRAK_CAMERA_H
#define WIATRAK_CAMERA_H

#include "glm/glm.hpp"

class Camera
{

    glm::vec3 position;         //position of camera

    glm::vec3 viewDirection;    //where camera is looking

    glm::mat4 projection;

    const glm::vec3 UP;

public:
    Camera();

    glm::mat4 getViewMatrix() const;

    void setViewDirection(const glm::vec3 &value);

    glm::vec3 getViewDirection() const;

    void setPosition(const glm::vec3 &value);

    glm::vec3 getPosition() const;

    glm::mat4 getProjectionMatrix() const;
};


#endif //WIATRAK_CAMERA_H