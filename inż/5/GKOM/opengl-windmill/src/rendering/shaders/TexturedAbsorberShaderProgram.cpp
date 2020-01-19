#include <iostream>
#include "TexturedAbsorberShaderProgram.h"

void TexturedAbsorberShaderProgram::setTexturedAbsorberMaterial(const TexturedAbsorber &texturedAbsorber) const
{
    setUniformVec3(MATERIAL_UNIFORM_NAME + ".diffuseColor", texturedAbsorber.getMaterial().diffuseColor.getVec3());
    setUniformVec3(MATERIAL_UNIFORM_NAME + ".specularColor", texturedAbsorber.getMaterial().specularColor.getVec3());
    setUniformFloat(MATERIAL_UNIFORM_NAME + ".shininess", texturedAbsorber.getMaterial().shininess);
    setUniformInt(MATERIAL_UNIFORM_NAME + ".texture_mode", texturedAbsorber.getTexture().getMode());
}

void TexturedAbsorberShaderProgram::setTexturedAbsorberTextureMode(const TexturedAbsorber &texturedAbsorber) const
{
    setUniformInt(TEXTURE_MODE_NAME, texturedAbsorber.getTexture().getMode());
}

void TexturedAbsorberShaderProgram::setViewPosition(const glm::vec3 &viewPosition) const
{
    setUniformVec3(VIEWPOS_UNIFORM_NAME, viewPosition);
}

void TexturedAbsorberShaderProgram::setLightsNumber(int lightsNumber) const
{
    setUniformInt(LIGHTSNUM_UNIFORM_NAME, lightsNumber);
}

void TexturedAbsorberShaderProgram::setLight(const PointLight &light, int lightIndex) const
{
    const auto lightIndexString = std::to_string(lightIndex);
    setUniformVec3(LIGHTS_UNIFORM_NAME + "[" + lightIndexString + "].position", light.getPosition());

    setUniformFloat(LIGHTS_UNIFORM_NAME + "[" + lightIndexString + "].constant", light.getAttributes().constant);
    setUniformFloat(LIGHTS_UNIFORM_NAME + "[" + lightIndexString + "].linear", light.getAttributes().linear);
    setUniformFloat(LIGHTS_UNIFORM_NAME + "[" + lightIndexString + "].quadratic", light.getAttributes().quadratic);

    setUniformVec3(LIGHTS_UNIFORM_NAME + "[" + lightIndexString + "].color", light.getAttributes().color.getVec3());

    setUniformFloat(LIGHTS_UNIFORM_NAME + "[" + lightIndexString + "].ambientIntensity",
                    light.getAttributes().ambientIntensity);
    setUniformFloat(LIGHTS_UNIFORM_NAME + "[" + lightIndexString + "].diffuseIntensity",
                    light.getAttributes().diffuseIntensity);
    setUniformFloat(LIGHTS_UNIFORM_NAME + "[" + lightIndexString + "].specularIntensity",
                    light.getAttributes().specularIntensity);
}