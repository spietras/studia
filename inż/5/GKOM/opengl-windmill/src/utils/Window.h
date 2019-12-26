#ifndef WIATRAK_WINDOW_H
#define WIATRAK_WINDOW_H

#include <string>
#include "GLFW/glfw3.h"

class Window
{
private:
    int width;
    int height;
    std::string title;
    GLFWwindow *handle;

public:
    Window(int width, int height, const std::string &title);

    int getWidth() const
    { return width; }

    int getHeight() const
    { return height; }

    void setActive() const;

    void resize(int newWidth, int newHeight);

    bool shouldClose() const;

    void draw() const;
};


#endif //WIATRAK_WINDOW_H
