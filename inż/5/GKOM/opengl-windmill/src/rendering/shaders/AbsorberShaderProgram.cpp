#include "AbsorberShaderProgram.h"

void AbsorberShaderProgram::setAbsorberColor(const Absorber &absorber) const
{
    setUniformVec4(COLOR_UNIFORM_NAME, absorber.getColor().getVec());
}