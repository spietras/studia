#ifndef ZT2_WIATRAK_RENDERER_H
#define ZT2_WIATRAK_RENDERER_H

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "Color.h"
#include "Scene.h"
#include "ShaderProgram.h"

class Renderer
{
    ColorFloat backgroundColor;

    void drawBackground() const;

    void drawScene(const Scene &scene, const ShaderProgram &shaderProgram) const;

public:

    Renderer(ColorFloat backgroundColor) : backgroundColor(backgroundColor)
    {};

    void render(const Scene &scene, const ShaderProgram &shaderProgram) const;
};


#endif //ZT2_WIATRAK_RENDERER_H
