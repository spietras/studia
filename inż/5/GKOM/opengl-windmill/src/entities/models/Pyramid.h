#ifndef WIATRAK_PYRAMID_H
#define WIATRAK_PYRAMID_H


#include "RectangleFrustum.h"

class Pyramid : public RectangleFrustum
{
public:
    Pyramid(float width,
            float depth,
            float height,
            int positionLocation,
            int normalLocation,
            int textureLocation,
            bool scaleTextures = true) : RectangleFrustum(width,
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
