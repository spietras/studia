#include "Renderer.h"

void Renderer::drawBackground() const
{
    glClearColor(backgroundColor.red,
                 backgroundColor.green,
                 backgroundColor.blue,
                 backgroundColor.alpha);
    glClear(GL_COLOR_BUFFER_BIT);
}

Renderer::Renderer(ColorFloat backgroundColor) : backgroundColor(backgroundColor)
{
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, GLFW_VERSION.MAJOR);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, GLFW_VERSION.MINOR);
    glewInit();
}

void Renderer::render(const Window &window) const
{
    drawBackground();
    window.draw();
}

void Renderer::cleanUp() const
{
    glfwTerminate();
}