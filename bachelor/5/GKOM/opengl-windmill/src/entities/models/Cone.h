#ifndef WIATRAK_CONE_H
#define WIATRAK_CONE_H


#include "CircularFrustum.h"

class Cone : public CircularFrustum
{
public:
    Cone(float radius,
         float height,
         int baseVertices,
         int positionLocation,
         int normalLocation,
         int textureLocation,
         bool scaleTextures = true) : CircularFrustum(radius,
                                                      0.0f,
                                                      height,
                                                      baseVertices,
                                                      positionLocation,
                                                      normalLocation,
                                                      textureLocation,
                                                      scaleTextures)
    {}
};


#endif //WIATRAK_CONE_H
