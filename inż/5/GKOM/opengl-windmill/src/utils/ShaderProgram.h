#ifndef WIATRAK_SHADERPROGRAM_H
#define WIATRAK_SHADERPROGRAM_H


#include <string>
#include "GL/glew.h"
#include "glm/glm.hpp"
#include "../entities/Entity.h"

class ShaderProgram
{
    const std::string MODEL_MATRIX_UNIFORM_NAME = "model";

    int shaderProgram;

    int createShader(int type, const std::string &source) const;

public:
    ShaderProgram(const std::string &vertexShaderSource, const std::string &fragmentShaderSource);

    void use() const;

    void setUniformBool(const std::string &name, bool value) const;

    void setUniformInt(const std::string &name, int value) const;

    void setUniformFloat(const std::string &name, float value) const;

    void setUniformVec2(const std::string &name, const glm::vec2 &value) const;

    void setUniformVec2(const std::string &name, float x, float y) const;

    void setUniformVec3(const std::string &name, const glm::vec3 &value) const;

    void setUniformVec3(const std::string &name, float x, float y, float z) const;

    void setUniformVec4(const std::string &name, const glm::vec4 &value) const;

    void setUniformVec4(const std::string &name, float x, float y, float z, float w) const;

    void setUniformMat2(const std::string &name, const glm::mat2 &mat) const;

    void setUniformMat3(const std::string &name, const glm::mat3 &mat) const;

    void setUniformMat4(const std::string &name, const glm::mat4 &mat) const;

    void applyEntityTransformation(const Entity &entity) const;
};


#endif //WIATRAK_SHADERPROGRAM_H
