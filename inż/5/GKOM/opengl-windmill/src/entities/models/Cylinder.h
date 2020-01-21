#ifndef WIATRAK_CYLINDER_H
#define WIATRAK_CYLINDER_H


#include "CircularFrustum.h"

class Cylinder : public CircularFrustum
{
public:
    Cylinder(float radius,
             float height,
             int baseVertices,
             int positionLocation,
             int normalLocation,
             int textureLocation,
             bool scaleTextures = true) : CircularFrustum(radius,
                                                          radius,
                                                          height,
                                                          baseVertices,
                                                          positionLocation,
                                                          normalLocation,
                                                          textureLocation,
                                                          scaleTextures)
    {}
};


#endif //WIATRAK_CYLINDER_H
