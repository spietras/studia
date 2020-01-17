#ifndef WIATRAK_WINDOW_H
#define WIATRAK_WINDOW_H

#define GLEW_STATIC

#include <string>
#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "Renderer.h"

class Window
{
    const struct
    {
        int MAJOR;
        int MINOR;
    } GLFW_VERSION = {3, 3};

    int width;
    int height;
    std::string title;
    GLFWwindow *handle;

public:
    Window(int width, int height, const std::string &title);
    ~Window();

    int getWidth() const
    { return width; }

    int getHeight() const
    { return height; }

    void resize(int newWidth, int newHeight);

    bool shouldClose() const;

    void draw(const Renderer &renderer, const Scene &scene, const ShaderProgram &shaderProgram) const;
};


#endif //WIATRAK_WINDOW_H
