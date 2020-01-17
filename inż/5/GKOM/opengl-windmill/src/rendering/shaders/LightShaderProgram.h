#ifndef WIATRAK_EMITTERSHADERPROGRAM_H
#define WIATRAK_EMITTERSHADERPROGRAM_H


#include "ShaderProgram.h"
#include "../../entities/lights/PointLight.h"

class LightShaderProgram : public ShaderProgram
{
    const std::string COLOR_UNIFORM_NAME = "lightColor";

public:
    LightShaderProgram(const std::string &vertexShaderPath, const std::string &fragmentShaderPath) : ShaderProgram(
            vertexShaderPath, fragmentShaderPath)
    {}

    void setLightColor(const PointLight &light) const;
};


#endif //WIATRAK_EMITTERSHADERPROGRAM_H
