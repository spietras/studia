#ifndef WIATRAK_ENTITY_H
#define WIATRAK_ENTITY_H


#include "glm/vec3.hpp"
#include "glm/mat4x4.hpp"
#include "glm/gtc/matrix_transform.hpp"
#include "models/BaseObjectModel.h"
#include "../utils/Color.h"

class Entity
{
    const BaseObjectModel &model; //geometry
    glm::mat4 modelMatrix; //transformation from local space to world space

public:
    Entity(const BaseObjectModel &model);

    const BaseObjectModel &getModel() const
    { return model; }

    const glm::mat4 &getModelMatrix() const
    { return modelMatrix; }

    void setPosition(const glm::vec3 &position);

    void translate(const glm::vec3 &vector); //apply translation to current transformation

    void rotateAroundOrigin(float radianAngle,
                            const glm::vec3 &axis); // apply rotation around world origin to current transformation

    void
    rotate(float radianAngle, const glm::vec3 &axis); //apply rotation around model origin to current transformation
};


#endif //WIATRAK_ENTITY_H
