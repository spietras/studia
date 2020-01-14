#ifndef WIATRAK_CUBOIDMODEL_H
#define WIATRAK_CUBOIDMODEL_H


#include "BaseObjectModel.h"

class CuboidModel : public BaseObjectModel
{
    static std::vector<glm::vec3> calculatePoints(float width, float height, float depth);

public:
    CuboidModel(float width, float height, float depth, int positionLocation);
};


#endif //WIATRAK_CUBOIDMODEL_H
