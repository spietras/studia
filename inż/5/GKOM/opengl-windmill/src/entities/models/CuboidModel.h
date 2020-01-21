#ifndef WIATRAK_CUBOIDMODEL_H
#define WIATRAK_CUBOIDMODEL_H

#include "BaseObjectModel.h"

class CuboidModel : public BaseObjectModel
{
    static std::vector<Vertex> calculateVertices(float width, float height, float depth, bool scaleTextures);

public:
    CuboidModel(float width,
                float height,
                float depth,
                int positionLocation,
                int normalLocation,
                int textureLocation,
                bool scaleTextures = false);
};

#endif //WIATRAK_CUBOIDMODEL_H
