#ifndef WIATRAK_CAMERA_H
#define WIATRAK_CAMERA_H

#include "glm/glm.hpp"

class Camera
{

    glm::vec3 position;         //position of camera

    glm::vec3 viewDirection;    //where camera is looking

    const glm::mat4 projection;

    const glm::vec3 UP;

    glm::vec2 oldMousePosition;      //where is the mouse

public:
    Camera();

    Camera(const unsigned int &SCR_WIDTH, const unsigned int &SCR_HEIGHT);

    glm::mat4 getViewMatrix() const;

    void updateMouse(const glm::vec2& newMousePosition);

    void setViewDirection(const glm::vec3 &value);

    glm::vec3 getViewDirection() const;

    void setPosition(const glm::vec3 &value);

    glm::vec3 getPosition() const;

    glm::mat4 getProjectionMatrix() const;

    glm::vec3 getUP() const;
};


#endif //WIATRAK_CAMERA_H