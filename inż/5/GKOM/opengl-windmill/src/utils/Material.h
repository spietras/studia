#ifndef WIATRAK_MATERIAL_H
#define WIATRAK_MATERIAL_H


#include "Color.h"

struct Material
{
    ColorFloat diffuseColor;
    ColorFloat specularColor;
    float shininess;

    Material(ColorFloat diffuseColor,
             ColorFloat specularColor,
             float shininess) : diffuseColor(diffuseColor),
                                specularColor(specularColor),
                                shininess(shininess)
    {}
};


#endif //WIATRAK_MATERIAL_H
