#ifndef WIATRAK_PLANEMODEL_H
#define WIATRAK_PLANEMODEL_H

#include "BaseObjectModel.h"

class PlaneModel : public BaseObjectModel
{
    static std::vector<Vertex> calculateVertices(float width, float height);

public:
    PlaneModel(float width, float height, int positionLocation, int normalLocation, int textureLocation);
};

#endif //WIATRAK_PLANEMODEL_H
