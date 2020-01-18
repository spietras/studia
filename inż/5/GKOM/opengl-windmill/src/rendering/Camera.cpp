#include "Camera.h"
#include <glm/gtx/transform.hpp>
#include <GL/glew.h>

Camera::Camera() : position (0.0f, 0.0f, 3.0f ), viewDirection (0.0f, 0.0f, -1.0f), UP(0.0f, 1.0f, 0.0f), projection (glm::perspective(glm::radians(45.0f), (GLfloat)600/(GLfloat)600, 0.1f, 100.0f))
{

}

glm::mat4 Camera::getViewMatrix() const
{
    return glm::lookAt(this->position, this->position + this->viewDirection, this->UP);
}

void Camera::setViewDirection(const glm::vec3 &value)
{
    this->viewDirection = value;
}

glm::vec3 Camera::getViewDirection() const
{
    return this->viewDirection;              
}

glm::vec3 Camera::getPosition() const
{
    return this->position;
}

void Camera::setPosition(const glm::vec3 &value)
{
    this->position = value;
}


glm::mat4 Camera::getProjectionMatrix() const
{
    return this->projection;
}
