#ifndef ZT2_WIATRAK_RENDERER_H
#define ZT2_WIATRAK_RENDERER_H

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "utils/Window.h"
#include "utils/Color.h"

class Renderer
{
    const struct
    {
        int MAJOR;
        int MINOR;
    } GLFW_VERSION = {3, 3};

    ColorFloat backgroundColor;

    void drawBackground() const;

public:

    Renderer(ColorFloat);

    void render(const Window &) const;

    void cleanUp() const;
};


#endif //ZT2_WIATRAK_RENDERER_H
