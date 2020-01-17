#include "Entity.h"

Entity::Entity(const BaseObjectModel &model) : model(model)
{
    this->modelMatrix = glm::mat4(1.0f); //start with identity matrix
}

void Entity::setPosition(const glm::vec3 &position)
{
    glm::vec3 currentPos = glm::vec3(this->modelMatrix * glm::vec4(0.0f, 0.0f, 0.0f, 1.0f));
    translate(-currentPos); //translate to origin
    translate(position); //translate to new position
}

void Entity::translate(const glm::vec3 &vector)
{
    glm::mat4 translationMatrix = glm::translate(glm::mat4(1.0f), vector);
    this->modelMatrix =
            translationMatrix * this->modelMatrix; //left side to be able to apply transformation chronologically
}

void Entity::rotateAroundOrigin(float radianAngle, const glm::vec3 &axis)
{
    glm::mat4 rotationMatrix = glm::rotate(glm::mat4(1.0f), radianAngle, axis);
    this->modelMatrix =
            rotationMatrix * this->modelMatrix; //left side to be able to apply transformation chronologically
}

void Entity::rotate(const float radianAngle, const glm::vec3 &axis)
{
    glm::vec3 currentPos = glm::vec3(this->modelMatrix * glm::vec4(0.0f, 0.0f, 0.0f, 1.0f));
    translate(-currentPos);
    rotateAroundOrigin(radianAngle, axis);
    translate(currentPos);
}