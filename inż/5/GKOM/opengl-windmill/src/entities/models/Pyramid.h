#ifndef WIATRAK_PYRAMID_H
#define WIATRAK_PYRAMID_H


#include "RectangularFrustum.h"

class Pyramid : public RectangularFrustum
{
public:
    Pyramid(float width,
            float depth,
            float height,
            int positionLocation,
            int normalLocation,
            int textureLocation,
            bool scaleTextures = true) : RectangularFrustum(width,
                                                          depth,
                                                          0.0f,
                                                          0.0f,
                                                          height,
                                                          positionLocation,
                                                          normalLocation,
                                                          textureLocation,
                                                          scaleTextures)
    {}
};


#endif //WIATRAK_PYRAMID_H
