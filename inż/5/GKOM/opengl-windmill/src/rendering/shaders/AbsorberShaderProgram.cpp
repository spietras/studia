#include <iostream>
#include "AbsorberShaderProgram.h"

void AbsorberShaderProgram::setAbsorberMaterial(const Absorber &absorber) const
{
    setUniformVec3(MATERIAL_UNIFORM_NAME + ".diffuseColor", absorber.getMaterial().diffuseColor.getVec3());
    setUniformVec3(MATERIAL_UNIFORM_NAME + ".specularColor", absorber.getMaterial().specularColor.getVec3());
    setUniformFloat(MATERIAL_UNIFORM_NAME + ".shininess", absorber.getMaterial().shininess);
}

void AbsorberShaderProgram::setViewPosition(const glm::vec3 &viewPosition) const
{
    setUniformVec3(VIEWPOS_UNIFORM_NAME, viewPosition);
}

void AbsorberShaderProgram::setLightsNumber(int lightsNumber) const
{
    setUniformInt(LIGHTSNUM_UNIFORM_NAME, lightsNumber);
}

void AbsorberShaderProgram::setLight(const PointLight &light, int lightIndex) const
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