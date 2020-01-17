#ifndef WIATRAK_ABSORBERSHADERPROGRAM_H
#define WIATRAK_ABSORBERSHADERPROGRAM_H


#include "ShaderProgram.h"
#include "../../entities/absorbers/Absorber.h"

class AbsorberShaderProgram : public ShaderProgram
{
    const std::string COLOR_UNIFORM_NAME = "objectColor";

public:
    AbsorberShaderProgram(const std::string &vertexShaderPath, const std::string &fragmentShaderPath) : ShaderProgram(
            vertexShaderPath, fragmentShaderPath)
    {}

    void setAbsorberColor(const Absorber &absorber) const;
};


#endif //WIATRAK_ABSORBERSHADERPROGRAM_H
