#ifndef WIATRAK_DEPTHSHADERPROGRAM_H
#define WIATRAK_DEPTHSHADERPROGRAM_H


#include "ShaderProgram.h"
#include "../../entities/lights/DirectionalLight.h"

class DepthShaderProgram : public ShaderProgram
{
    // shader uniform variables
    const std::string LIGHTSPACE_MATRIX_UNIFORM_NAME = "lightSpaceMatrix";

public:
    DepthShaderProgram(const std::string &vertexShaderPath, const std::string &fragmentShaderPath) : ShaderProgram(
            vertexShaderPath, fragmentShaderPath)
    {}

    void applyLightSpace(const DirectionalLight &directionalLight) const;
};


#endif //WIATRAK_DEPTHSHADERPROGRAM_H
