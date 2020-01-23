#ifndef WIATRAK_REGULARTETRAHEDRON_H
#define WIATRAK_REGULARTETRAHEDRON_H


#include "Tetrahedron.h"

class RegularTetrahedron : public Tetrahedron
{
public:
    RegularTetrahedron(float side,
                       int positionLocation,
                       int normalLocation,
                       int textureLocation,
                       bool scaleTextures = true) : Tetrahedron(side,
                                                                side * sqrtf(6) / 3.0f,
                                                                positionLocation,
                                                                normalLocation,
                                                                textureLocation,
                                                                scaleTextures)
    {}
};


#endif //WIATRAK_REGULARTETRAHEDRON_H
