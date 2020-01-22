#ifndef WIATRAK_ENTITY_H
#define WIATRAK_ENTITY_H


#include "glm/vec3.hpp"
#include "glm/mat4x4.hpp"
#include "glm/gtc/quaternion.hpp"
#include "glm/gtc/matrix_transform.hpp"
#include "models/BaseObjectModel.h"
#include "../utils/Color.h"

class Entity
{
    const float SCALE_EPS = 0.01f;

    const BaseObjectModel &model; //geometry
    glm::mat4 modelMatrix; //transformation from local space to world space

    glm::vec3 currentScale;
    glm::quat currentRotation;
    std::vector<Entity*> children;

public:
    Entity(const BaseObjectModel &model);

    const BaseObjectModel &getModel() const
    { return model; }

    const glm::mat4 &getModelMatrix() const
    { return modelMatrix; }

    glm::vec3 getPosition() const;

    void setPosition(const glm::vec3 &position);

    void translate(const glm::vec3 &vector); //apply translation to current transformation

    glm::quat getRotation() const;

    void setRotation(float radianAngle, const glm::vec3 &axis);

    void setRotation(const glm::quat &rotation);

    void setRotationRelative(const glm::vec3 &pivot, float radianAngle, const glm::vec3 &axis);

    void setRotationRelative(const glm::vec3 &pivot, const glm::quat &rotation);

    void rotateAroundOrigin(float radianAngle,
                            const glm::vec3 &axis); // apply rotation around world origin to current transformation

    void rotateAroundOrigin(const glm::quat &rotation); // apply rotation around world origin to current transformation

    void
    rotate(float radianAngle, const glm::vec3 &axis); //apply rotation around model origin to current transformation

    void
    rotate(const glm::quat &rotation); //apply rotation around model origin to current transformation

    void
    rotateRelative(const glm::vec3 &pivot, float radianAngle, const glm::vec3 &axis); //apply rotation around model origin to current transformation

    void
    rotateRelative(const glm::vec3 &pivot, const glm::quat &rotation); //apply rotation around model origin to current transformation

    void
    rotateLocal(float radianAngle, const glm::vec3 &axis); //apply rotation around model origin to current transformation

    void
    rotateLocal(const glm::quat &rotation); //apply rotation around model origin to current transformation

    glm::vec3 getScale() const;

    void setScale(const glm::vec3 &factor);

    void setScale(float factor);

    void setScaleRelative(const glm::vec3 &pivot, const glm::vec3 &factor);

    void setScaleRelative(const glm::vec3 &pivot, float factor);

    void scaleFromOrigin(const glm::vec3 &factor);

    void scaleFromOrigin(float factor);

    void scale(const glm::vec3 &factor);

    void scale(float factor);

    void scaleRelative(const glm::vec3 &pivot, const glm::vec3 &factor);

    void scaleRelative(const glm::vec3 &pivot, float factor);

    void addChild(Entity* object);
    };


#endif //WIATRAK_ENTITY_H
