#include "LightShaderProgram.h"

void LightShaderProgram::setLightColor(const PointLight &light) const
{
    setUniformVec4(COLOR_UNIFORM_NAME, light.getAttributes().color.getVec4() * light.getAttributes().globalIntensity);
}