#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "Renderer.h"
#include <iostream>

int main()
{
    const unsigned int SCR_WIDTH = 800;
    const unsigned int SCR_HEIGHT = 600;
    const std::string TITLE = "Wiatrak";
    const ColorFloat BG_COLOR = {0.2f, 0.3f, 0.3f, 1.0f};

    Renderer r(BG_COLOR);
    Window w(SCR_WIDTH, SCR_HEIGHT, TITLE);
    w.setActive();

    while (!w.shouldClose())
        r.render(w);

    r.cleanUp();
    return 0;
}