#include "ShaderProgram.h"

int ShaderProgram::createShader(int type, const std::string &source) const
{
    int shader = glCreateShader(type);
    const char *c_str = source.c_str();
    glShaderSource(shader, 1, &c_str, nullptr);
    glCompileShader(shader);
    return shader;
}


ShaderProgram::ShaderProgram(const std::string &vertexShaderSource, const std::string &fragmentShaderSource)
{
    int vertexShader = createShader(GL_VERTEX_SHADER, vertexShaderSource);
    int fragmentShader = createShader(GL_FRAGMENT_SHADER, fragmentShaderSource);

    this->shaderProgram = glCreateProgram();
    glAttachShader(this->shaderProgram, vertexShader);
    glAttachShader(this->shaderProgram, fragmentShader);
    glLinkProgram(this->shaderProgram);

    //clean
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);
}

void ShaderProgram::use() const
{
    glUseProgram(this->shaderProgram);
}

void ShaderProgram::setUniformBool(const std::string &name, bool value) const
{
    glProgramUniform1i(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), (int) value);
}

void ShaderProgram::setUniformInt(const std::string &name, int value) const
{
    glProgramUniform1i(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), value);
}

void ShaderProgram::setUniformFloat(const std::string &name, float value) const
{
    glProgramUniform1f(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), value);
}

void ShaderProgram::setUniformVec2(const std::string &name, const glm::vec2 &value) const
{
    glProgramUniform2fv(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), 1, &value[0]);
}

void ShaderProgram::setUniformVec2(const std::string &name, float x, float y) const
{
    glProgramUniform2f(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), x, y);
}

void ShaderProgram::setUniformVec3(const std::string &name, const glm::vec3 &value) const
{
    glProgramUniform3fv(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), 1, &value[0]);
}

void ShaderProgram::setUniformVec3(const std::string &name, float x, float y, float z) const
{
    glProgramUniform3f(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), x, y, z);
}

void ShaderProgram::setUniformVec4(const std::string &name, const glm::vec4 &value) const
{
    glProgramUniform4fv(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), 1, &value[0]);
}

void ShaderProgram::setUniformVec4(const std::string &name, float x, float y, float z, float w) const
{
    glProgramUniform4f(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), x, y, z, w);
}

void ShaderProgram::setUniformMat2(const std::string &name, const glm::mat2 &mat) const
{
    glProgramUniformMatrix2fv(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), 1, GL_FALSE,
                              &mat[0][0]);
}

void ShaderProgram::setUniformMat3(const std::string &name, const glm::mat3 &mat) const
{
    glProgramUniformMatrix3fv(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), 1, GL_FALSE,
                              &mat[0][0]);
}

void ShaderProgram::setUniformMat4(const std::string &name, const glm::mat4 &mat) const
{
    glProgramUniformMatrix4fv(this->shaderProgram, glGetUniformLocation(this->shaderProgram, name.c_str()), 1, GL_FALSE,
                              &mat[0][0]);
}

void ShaderProgram::applyEntityTransformation(const Entity &entity) const
{
    setUniformMat4(MODEL_MATRIX_UNIFORM_NAME, entity.getModelMatrix()); //copy model matrix to uniform in shader
}
