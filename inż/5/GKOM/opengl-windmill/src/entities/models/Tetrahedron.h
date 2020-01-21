#ifndef WIATRAK_TETRAHEDRON_H
#define WIATRAK_TETRAHEDRON_H


#include "Cone.h"

class Tetrahedron : public Cone
{
public:
    Tetrahedron(float side,
                float height,
                int positionLocation,
                int normalLocation,
                int textureLocation,
                bool scaleTextures = true) : Cone(side * sqrtf(3) / 3.0f,
                                                  height,
                                                  3,
                                                  positionLocation,
                                                  normalLocation,
                                                  textureLocation,
                                                  scaleTextures)
    {}
};


#endif //WIATRAK_TETRAHEDRON_H
