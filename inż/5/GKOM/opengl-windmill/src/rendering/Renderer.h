#ifndef ZT2_WIATRAK_RENDERER_H
#define ZT2_WIATRAK_RENDERER_H

#define GLEW_STATIC

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "../utils/Color.h"
#include "Scene.h"
#include "shaders/AbsorberShaderProgram.h"
#include "shaders/LightShaderProgram.h"
#include "shaders/DepthShaderProgram.h"

class Renderer
{
    ColorFloat backgroundColor;

    void drawBackground() const;

    void drawSceneAbsorbersDepth(const Scene& scene, const DepthShaderProgram& shaderProgram) const;

    void drawEntity(const Entity &entity) const;

    void drawSceneAbsorbers(const Scene &scene, const Camera &camera, const AbsorberShaderProgram &shaderProgram) const;

    void drawSceneLights(const Scene &scene, const LightShaderProgram &shaderProgram) const;

public:

    Renderer(ColorFloat backgroundColor) : backgroundColor(backgroundColor)
    {};

    void renderShadowMap(const Scene &scene, const DepthShaderProgram &depthShaderProgram) const;

    void render(const Scene &scene, const Camera &camera,
                const AbsorberShaderProgram &absorberShaderProgram,
                const LightShaderProgram &lightShaderProgram) const;
};


#endif //ZT2_WIATRAK_RENDERER_H
