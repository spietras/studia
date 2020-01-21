#ifndef WIATRAK_CUBOIDMODEL_H
#define WIATRAK_CUBOIDMODEL_H

#include "RectangleFrustum.h"

class CuboidModel : public RectangleFrustum
{
public:
    CuboidModel(float width,
                float height,
                float depth,
                int positionLocation,
                int normalLocation,
                int textureLocation,
                bool scaleTextures = true) : RectangleFrustum(width,
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
