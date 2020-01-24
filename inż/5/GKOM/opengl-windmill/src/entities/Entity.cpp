#include "Entity.h"

Entity::Entity(const BaseObjectModel &model) : model(&model) {}

Entity::Entity(const BaseObjectModel *model) : model(model)
{
    this->modelMatrix = glm::mat4(1.0f); //start with identity matrix
    this->currentScale = glm::vec3(1.0f);
    this->currentRotation = glm::quat(1.0f, 0.0f, 0.0f, 0.0f);
}

glm::vec3 Entity::getPosition() const
{
    return glm::vec3(this->modelMatrix * glm::vec4{0.0f, 0.0f, 0.0f, 1.0f});
}

void Entity::setPosition(const glm::vec3 &position)
{
    glm::vec3 currentPos = getPosition();
    translate(-currentPos); //translate to origin
    translate(position);    //translate to new position
}

void Entity::translate(const glm::vec3 &vector)
{
    glm::mat4 translationMatrix = glm::translate(glm::mat4(1.0f), vector);
    this->modelMatrix =
        translationMatrix * this->modelMatrix; //left side to be able to apply transformation chronologically

    for(Entity* child : children)
        child->translate(vector);
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

    for(Entity* child : children)
        child->rotateAroundOrigin(rotation);
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
    if (factor.x <= SCALE_EPS || factor.y <= SCALE_EPS || factor.z <= SCALE_EPS) //prevent negative and zero scale
        return;

    scale({1.0f / this->currentScale.x, 1.0f / this->currentScale.y, 1.0f / this->currentScale.z}); //scale to 1
    scale(factor);
}

void Entity::setScale(float factor)
{
    setScale(glm::vec3(factor, factor, factor));
}

void Entity::scaleFromOrigin(const glm::vec3 &factor)
{
    glm::mat4 scaleMatrix = glm::scale(glm::mat4(1.0f), factor);
    this->modelMatrix =
        scaleMatrix * this->modelMatrix; //left side to be able to apply transformation chronologically
    this->currentScale *= factor;

    for(Entity* child : children)
        child->scaleFromOrigin(factor);
}

void Entity::scaleFromOrigin(float factor)
{
    scaleFromOrigin(glm::vec3(factor, factor, factor));
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

void Entity::scale(float factor)
{
    scale(glm::vec3(factor, factor, factor));
}

void Entity::addChild(Entity *obj)
{
    children.push_back(obj);
}

void Entity::removeChild(Entity *obj)
{
    children.erase(std::remove(children.begin(), children.end(), obj), children.end());
}


void Entity::setRotationRelative(const glm::vec3 &pivot, float radianAngle, const glm::vec3 &axis)
{
    setRotationRelative(pivot, glm::quat(axis * radianAngle));
}

void Entity::setRotationRelative(const glm::vec3 &pivot, const glm::quat &rotation)
{
    translate(-pivot);
    setRotation(rotation);
    translate(pivot);
}

void Entity::rotateRelative(const glm::vec3 &pivot, float radianAngle, const glm::vec3 &axis)
{
    rotateRelative(pivot, glm::quat(axis * radianAngle));
}

void Entity::rotateRelative(const glm::vec3 &pivot, const glm::quat &rotation)
{
    translate(-pivot);
    rotateAroundOrigin(rotation);
    translate(pivot);
}

void Entity::setScaleRelative(const glm::vec3 &pivot, const glm::vec3 &factor)
{
    translate(-pivot);
    setScale(factor);
    translate(pivot);
}

void Entity::setScaleRelative(const glm::vec3 &pivot, float factor)
{
    setScaleRelative(pivot, glm::vec3(factor, factor, factor));
}

void Entity::scaleRelative(const glm::vec3 &pivot, const glm::vec3 &factor)
{
    translate(-pivot);
    scaleFromOrigin(factor);
    translate(pivot);
}

void Entity::scaleRelative(const glm::vec3 &pivot, float factor)
{
    scaleRelative(pivot, glm::vec3(factor, factor, factor));
}

void Entity::rotateLocal(float radianAngle, const glm::vec3 &axis)
{
    rotateLocal(glm::quat(axis * radianAngle));
}

void Entity::rotateLocal(const glm::quat &rotation)
{
    glm::quat currentRot = getRotation();
    glm::quat reverseRotation = glm::conjugate(currentRot);

    rotate(reverseRotation);
    rotate(rotation);
    rotate(currentRot);
}

void Entity::setRotationLocal(float radianAngle, const glm::vec3 &axis)
{
    setRotation(glm::quat(axis * radianAngle));
}

void Entity::setRotationLocal(const glm::quat &rotation)
{
    glm::quat currentRot = getRotation();
    glm::quat reverseRotation = glm::conjugate(currentRot);

    rotate(reverseRotation);
    setRotation(rotation);
    rotate(currentRot);
}

