#ifndef WIATRAK_SCENE_H
#define WIATRAK_SCENE_H


#include <vector>
#include "../entities/Entity.h"

class Scene
{
    std::vector<const Entity*> entities; //containing pointers, because we create and modify entities outside Scene

public:
    const std::vector<const Entity*> &getEntities() const;

    void addEntity(const Entity &entity);
};


#endif //WIATRAK_SCENE_H
