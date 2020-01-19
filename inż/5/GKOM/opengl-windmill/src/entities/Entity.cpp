#include <iostream>
#include "Entity.h"

Entity::Entity(const BaseObjectModel &model) : model(model)
{
    this->modelMatrix = glm::mat4(1.0f); //start with identity matrix
    this->currentPosition = glm::vec3(0.0f);
    this->currentScale = glm::vec3(1.0f);
    this->currentRotation = glm::quat(1.0f, 0.0f, 0.0f, 0.0f);
}

glm::vec3 Entity::getPosition() const
{
    return this->currentPosition;
}

void Entity::setPosition(const glm::vec3 &position)
{
    glm::vec3 currentPos = getPosition();
    translate(-currentPos); //translate to origin
    translate(position); //translate to new position
}

void Entity::translate(const glm::vec3 &vector)
{
    glm::mat4 translationMatrix = glm::translate(glm::mat4(1.0f), vector);
    this->modelMatrix =
            translationMatrix * this->modelMatrix; //left side to be able to apply transformation chronologically
    this->currentPosition += vector;
}

glm::quat Entity::getRotation() const
{
    return this->currentRotation;
}

void Entity::setRotation(float radianAngle, const glm::vec3 &axis)
{
    setRotation(glm::quat(axis * radianAngle));
}

void Entity::setRotation(const glm::quat &rotation)
{
    glm::quat reverseRotation = glm::conjugate(this->currentRotation);
    rotate(reverseRotation);
    rotate(rotation);
}

void Entity::rotateAroundOrigin(float radianAngle, const glm::vec3 &axis)
{
    rotateAroundOrigin(glm::quat(axis * radianAngle));
}

void Entity::rotateAroundOrigin(const glm::quat &rotation)
{
    glm::quat unitRotation = glm::normalize(rotation);
    glm::mat4 rotationMatrix = glm::mat4_cast(unitRotation);
    this->modelMatrix =
            rotationMatrix * this->modelMatrix; //left side to be able to apply transformation chronologically
    this->currentRotation = glm::normalize(unitRotation * this->currentRotation);
}

void Entity::rotate(const float radianAngle, const glm::vec3 &axis)
{
    rotate(glm::quat(axis * radianAngle));
}

void Entity::rotate(const glm::quat &rotation)
{
    glm::vec3 currentPos = getPosition();
    translate(-currentPos);
    rotateAroundOrigin(rotation);
    translate(currentPos);
}

glm::vec3 Entity::getScale() const
{
    return currentScale;
}

void Entity::setScale(const glm::vec3 &factor)
{
    if(factor.x <= 0.0f || factor.y <= SCALE_EPS || factor.z <= SCALE_EPS) //prevent negative and zero scale
        return;

    scale({1.0f/this->currentScale.x, 1.0f/this->currentScale.y, 1.0f/this->currentScale.z}); //scale to 1
    scale(factor);
}

void Entity::scaleFromOrigin(const glm::vec3 &factor)
{
    glm::mat4 scaleMatrix = glm::scale(glm::mat4(1.0f), factor);
    this->modelMatrix =
            scaleMatrix * this->modelMatrix; //left side to be able to apply transformation chronologically
    this->currentScale *= factor;
}

void Entity::scale(const glm::vec3 &factor)
{
    glm::vec3 currentPos = getPosition();
    glm::quat rotation = getRotation();
    glm::quat reverseRotation = glm::conjugate(rotation);
    translate(-currentPos);
    rotate(reverseRotation);
    scaleFromOrigin(factor);
    rotate(rotation);
    translate(currentPos);
}