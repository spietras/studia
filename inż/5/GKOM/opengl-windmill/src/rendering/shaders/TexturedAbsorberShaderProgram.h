#ifndef WIATRAK_TEXTUREDABSORBERSHADERPROGRAM_H
#define WIATRAK_TEXTUREDABSORBERSHADERPROGRAM_H

#include "ShaderProgram.h"
#include "../../entities/absorbers/TexturedAbsorber.h"
#include "../../entities/lights/PointLight.h"
#include "../../entities/skybox/Skybox.h"

class TexturedAbsorberShaderProgram : public ShaderProgram
{
    const std::string MATERIAL_UNIFORM_NAME = "material";
    const std::string VIEWPOS_UNIFORM_NAME = "viewPos";
    const std::string LIGHTSNUM_UNIFORM_NAME = "lightsNum";
    const std::string LIGHTS_UNIFORM_NAME = "pointLights";
    const std::string TEXTURE_MODE_NAME = "textureMode";

public:
    TexturedAbsorberShaderProgram(const std::string &vertexShaderPath, const std::string &fragmentShaderPath) : ShaderProgram(
                                                                                                                    vertexShaderPath, fragmentShaderPath)
    {
    }
    void setTexturedAbsorberColor(const TexturedAbsorber &texturedAbsorber) const;

    void setTexturedAbsorberTextureMode(const TexturedAbsorber &texturedAbsorber) const;

    void setTexturedAbsorberMaterial(const TexturedAbsorber &texturedAbsorber) const;

    void setViewPosition(const glm::vec3 &viewPosition) const;

    void setLightsNumber(int lightsNumber) const;

    void setLight(const PointLight &light, int lightIndex) const;
};
#endif // WIATRAK_TEXTUREDABSORBERSHADERPROGRAM_H