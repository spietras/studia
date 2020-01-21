#ifndef WIATRAK_PLANEMODEL_H
#define WIATRAK_PLANEMODEL_H

#include "BaseObjectModel.h"

class PlaneModel : public BaseObjectModel
{
    static std::vector<Vertex> calculateVertices(float width, float depth, bool scaleTextures);

public:
    PlaneModel(float width,
               float depth,
               int positionLocation,
               int normalLocation,
               int textureLocation,
               bool scaleTextures = true);
};

#endif //WIATRAK_PLANEMODEL_H
