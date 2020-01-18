#ifndef ZT2_WIATRAK_RENDERER_H
#define ZT2_WIATRAK_RENDERER_H

#define GLEW_STATIC

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "../utils/Color.h"
#include "Scene.h"
#include "shaders/AbsorberShaderProgram.h"
#include "shaders/LightShaderProgram.h"
#include "shaders/SkyboxShaderProgram.h"

class Renderer
{
    ColorFloat backgroundColor;

    void drawBackground() const;

    void drawEntity(const Entity &entity) const;

    void drawSceneAbsorbers(const Scene &scene, const Camera &camera, const AbsorberShaderProgram &shaderProgram) const;

    void drawSceneLights(const Scene &scene, const LightShaderProgram &shaderProgram) const;

    void drawSceneSkybox(const Scene &scene, const SkyboxShaderProgram &shaderProgram) const;

public:
    Renderer(ColorFloat backgroundColor) : backgroundColor(backgroundColor){};

    void render(const Scene &scene, const AbsorberShaderProgram &absorberShaderProgram,
                const LightShaderProgram &lightShaderProgram, const SkyboxShaderProgram &skyboxShaderProgram, const Camera& camera) const;
};

#endif //ZT2_WIATRAK_RENDERER_H
