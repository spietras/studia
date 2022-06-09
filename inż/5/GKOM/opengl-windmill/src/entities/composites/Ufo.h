#ifndef WIATRAK_UFO_H
#define WIATRAK_UFO_H


#include "../CompositeEntity.h"
#include "../models/CircularFrustum.h"

class Ufo : public CompositeEntity
{
    Material *material = nullptr;
    CircularFrustum *upModel = nullptr;
    CircularFrustum *downModel = nullptr;
    Sphere *lightModel = nullptr;

    std::vector<Absorber *> createAbsorbers(float radius,
                                            int positionLocation,
                                            int normalLocation,
                                            int textureLocation)
    {
        std::vector<Absorber *> absorbers;

        material = new Material(ColorInt(255, 255, 255), ColorInt(255, 255, 255), 32.0f);

        float upHeight = radius / 3.0f;
        float downHeight = radius / 6.0f;

        upModel = new CircularFrustum(radius, radius * 2.0f / 3.0f, upHeight, 10, positionLocation, normalLocation,
                                      textureLocation);
        downModel = new CircularFrustum(radius / 3.0f, radius, downHeight, 10, positionLocation, normalLocation,
                                        textureLocation);

        auto *up = new Absorber(*upModel, *material);
        auto *down = new Absorber(*downModel, *material);

        up->translate({0.0f, upHeight * 0.5f, 0.0f});
        down->translate({0.0f, -downHeight * 0.5f, 0.0f});

        absorbers.push_back(up);
        absorbers.push_back(down);

        return absorbers;
    }

    std::vector<PointLight *> createLights(float radius,
                                           int positionLocation,
                                           int normalLocation,
                                           int textureLocation)
    {
        std::vector<PointLight *> lights;

        float sphereRadius = radius * 0.1f;

        lightModel = new Sphere(sphereRadius, 50, 50, positionLocation, normalLocation, textureLocation);

        auto *downLight = new PointLight(*lightModel, PointLightAttributes(ColorInt(0, 255, 0),
                                                                           0.2f, 1.0f, 0.2f,
                                                                           1.0f, 0.22f, 0.2f));

        lights.push_back(downLight);

        float downHeight = radius / 6.0f;

        downLight->translate({0.0f, -downHeight, 0.0f});

        return lights;
    }

public:
    Ufo(float radius,
        int positionLocation,
        int normalLocation,
        int textureLocation) : CompositeEntity(createAbsorbers(radius,
                                                               positionLocation,
                                                               normalLocation,
                                                               textureLocation),
                                               createLights(radius,
                                                            positionLocation,
                                                            normalLocation,
                                                            textureLocation))
    {}

    ~Ufo()
    {
        delete material;
        delete upModel;
        delete downModel;
    }
};


#endif //WIATRAK_UFO_H
