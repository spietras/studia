#ifndef WIATRAK_CUBEMODEL_H
#define WIATRAK_CUBEMODEL_H


#include "CuboidModel.h"

class CubeModel : public CuboidModel
{
public:
    CubeModel(float edgeSize, int positionLocation) : CuboidModel(edgeSize, edgeSize, edgeSize, positionLocation)
    {}
};


#endif //WIATRAK_CUBEMODEL_H
