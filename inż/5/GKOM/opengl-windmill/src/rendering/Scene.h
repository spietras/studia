#ifndef WIATRAK_SCENE_H
#define WIATRAK_SCENE_H


#include <vector>
#include "../entities/Entity.h"
#include "../entities/absorbers/Absorber.h"
#include "../entities/lights/PointLight.h"

class Scene
{
    //containing pointers, because we create and modify entities outside Scene
    std::vector<const Absorber *> absorbers;
    std::vector<const PointLight *> lights;

public:
    const std::vector<const Absorber *> &getAbsorbers() const;

    void addAbsorber(const Absorber &absorber);

    const std::vector<const PointLight *> &getLights() const;

    void addLight(const PointLight &light);
};


#endif //WIATRAK_SCENE_H
