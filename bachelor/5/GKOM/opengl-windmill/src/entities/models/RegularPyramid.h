#ifndef WIATRAK_REGULARPYRAMID_H
#define WIATRAK_REGULARPYRAMID_H


#include "Pyramid.h"

class RegularPyramid : public Pyramid
{
public:
    RegularPyramid(float width,
                   float height,
                   int positionLocation,
                   int normalLocation,
                   int textureLocation,
                   bool scaleTextures = true) : Pyramid(width,
                                                        width,
                                                        height,
                                                        positionLocation,
                                                        normalLocation,
                                                        textureLocation,
                                                        scaleTextures)
    {}
};


#endif //WIATRAK_REGULARPYRAMID_H
