#ifndef WIATRAK_CUBOIDMODEL_H
#define WIATRAK_CUBOIDMODEL_H

#include "RectangularFrustum.h"

class CuboidModel : public RectangularFrustum
{
public:
    CuboidModel(float width,
                float height,
                float depth,
                int positionLocation,
                int normalLocation,
                int textureLocation,
                bool scaleTextures = true) : RectangularFrustum(width,
                                                              depth,
                                                              width,
                                                              depth,
                                                              height,
                                                              positionLocation,
                                                              normalLocation,
                                                              textureLocation,
                                                              scaleTextures)
    {}
};

#endif //WIATRAK_CUBOIDMODEL_H
