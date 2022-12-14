#include <iostream>
#include "SkyboxShaderProgram.h"

void SkyboxShaderProgram::setSkyboxColor(const Skybox &skybox) const
{
    setUniformVec4("objectColor", glm::vec4(1.0f, 0.0f, 0.0f, 1.0f));
}

void SkyboxShaderProgram::setSkyboxTexture(const Skybox &skybox) const
{
    setUniformVec4("objectColor", glm::vec4(1.0f, 0.0f, 0.0f, 1.0f));
}

void SkyboxShaderProgram::setSkyboxMaterial(const Skybox &skybox) const
{
    setUniformVec3(MATERIAL_UNIFORM_NAME + ".diffuseColor", skybox.getMaterial().diffuseColor.getVec3());
    setUniformVec3(MATERIAL_UNIFORM_NAME + ".specularColor", skybox.getMaterial().specularColor.getVec3());
    setUniformFloat(MATERIAL_UNIFORM_NAME + ".shininess", skybox.getMaterial().shininess);
}

void SkyboxShaderProgram::setViewPosition(const glm::vec3 &viewPosition) const
{
    setUniformVec3(VIEWPOS_UNIFORM_NAME, viewPosition);
}

void SkyboxShaderProgram::setLightsNumber(int lightsNumber) const
{
    setUniformInt(LIGHTSNUM_UNIFORM_NAME, lightsNumber);
}

void SkyboxShaderProgram::setLight(const PointLight &light, int lightIndex) const
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