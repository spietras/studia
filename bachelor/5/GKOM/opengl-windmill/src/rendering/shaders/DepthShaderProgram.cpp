#include "DepthShaderProgram.h"

void DepthShaderProgram::applyLightSpace(const DirectionalLight &directionalLight) const
{
    setUniformMat4(LIGHTSPACE_MATRIX_UNIFORM_NAME, directionalLight.getLightSpaceMatrix());
}
