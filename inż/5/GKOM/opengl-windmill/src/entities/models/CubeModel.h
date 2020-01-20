#ifndef WIATRAK_CUBEMODEL_H
#define WIATRAK_CUBEMODEL_H

#include "CuboidModel.h"

class CubeModel : public CuboidModel
{
public:
    CubeModel(float edgeSize, int positionLocation, int normalLocation, int textureLocation) : CuboidModel(edgeSize, edgeSize, edgeSize,
                                                                                                           positionLocation, normalLocation, textureLocation)
    {
    }
};

#endif //WIATRAK_CUBEMODEL_H
