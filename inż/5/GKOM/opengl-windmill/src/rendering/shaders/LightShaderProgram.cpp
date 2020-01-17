#include "LightShaderProgram.h"

void LightShaderProgram::setLightColor(const PointLight &absorber) const
{
    setUniformVec4(COLOR_UNIFORM_NAME, absorber.getColor().getVec());
}