#ifndef WIATRAK_SHADERPROGRAM_H
#define WIATRAK_SHADERPROGRAM_H

#define GLEW_STATIC

#include <string>
#include "GL/glew.h"
#include "glm/glm.hpp"
#include "../../entities/Entity.h"

class ShaderProgram
{
    // shader uniform variables
    const std::string MODEL_MATRIX_UNIFORM_NAME = "model";

    int shaderProgram;

    static std::string getShaderSource(const std::string &path);

    static int createShader(int type, const std::string &source);

public:
    ShaderProgram(const std::string &vertexShaderPath, const std::string &fragmentShaderPath);

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
