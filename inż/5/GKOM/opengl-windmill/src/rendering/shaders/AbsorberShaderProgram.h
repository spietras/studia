#ifndef WIATRAK_ABSORBERSHADERPROGRAM_H
#define WIATRAK_ABSORBERSHADERPROGRAM_H

#include "ShaderProgram.h"
#include "../../entities/absorbers/Absorber.h"
#include "../../entities/lights/PointLight.h"
#include "../../entities/lights/DirectionalLight.h"

class AbsorberShaderProgram : public ShaderProgram
{
    // shader uniform variables
    const std::string MATERIAL_UNIFORM_NAME = "material";
    const std::string VIEWPOS_UNIFORM_NAME = "viewPos";
    const std::string LIGHTSNUM_UNIFORM_NAME = "lightsNum";
    const std::string LIGHTS_UNIFORM_NAME = "pointLights";
    const std::string DIRECTIONAL_LIGHT_UNIFORM_NAME = "directionalLight";
    const std::string MODE = "mode";
    const std::string TEXTURE_MODE = "textureMode";

public:
    AbsorberShaderProgram(const std::string &vertexShaderPath, const std::string &fragmentShaderPath) : ShaderProgram(
                                                                                                            vertexShaderPath, fragmentShaderPath)
    {
    }

    void setAbsorberMaterial(const Absorber &absorber) const;

    void setMode(const Absorber &absorber) const;

    void setTextureMode(const Absorber &absorber) const;

    void setViewPosition(const glm::vec3 &viewPosition) const;

    void setLightsNumber(int lightsNumber) const;

    void setLight(const PointLight &light, int lightIndex) const;

    void setDirectionlight(const DirectionalLight &light) const;
};

#endif //WIATRAK_ABSORBERSHADERPROGRAM_H
