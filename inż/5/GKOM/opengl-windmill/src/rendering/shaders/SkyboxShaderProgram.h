#ifndef WIATRAK_SKYBOXSHADERPROGRAM_H
#define WIATRAK_SKYBOXSHADERPROGRAM_H

#include "ShaderProgram.h"
#include "../../entities/absorbers/Absorber.h"
#include "../../entities/lights/PointLight.h"
#include "../../entities/skybox/Skybox.h"

class SkyboxShaderProgram : public ShaderProgram
{
    const std::string MATERIAL_UNIFORM_NAME = "material";
    const std::string VIEWPOS_UNIFORM_NAME = "viewPos";
    const std::string LIGHTSNUM_UNIFORM_NAME = "lightsNum";
    const std::string LIGHTS_UNIFORM_NAME = "pointLights";

public:
    SkyboxShaderProgram(const std::string &vertexShaderPath, const std::string &fragmentShaderPath) : ShaderProgram(
                                                                                                          vertexShaderPath, fragmentShaderPath)
    {
    }
    void setSkyboxColor(const Skybox &skybox) const;
    void setSkyboxTexture(const Skybox &skybox) const;

    void setSkyboxMaterial(const Skybox &skybox) const;

    void setViewPosition(const glm::vec3 &viewPosition) const;

    void setLightsNumber(int lightsNumber) const;

    void setLight(const PointLight &light, int lightIndex) const;
};
#endif // WIATRAK_SKYBOXSHADERPROGRAM_H