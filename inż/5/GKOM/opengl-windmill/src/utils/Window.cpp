#include "Window.h"

Window::Window(int width, int height, const std::string &title) : width(width), height(height), title(title)
{
    this->handle = glfwCreateWindow(width, height, title.c_str(), nullptr, nullptr);

    glViewport(0, 0, width, height);
}

void Window::setActive() const
{
    glfwMakeContextCurrent(this->handle);
}

void Window::resize(int newWidth, int newHeight)
{
    this->width = newWidth;
    this->height = newHeight;
    glViewport(0, 0, newWidth, newHeight);
}

bool Window::shouldClose() const
{
    return glfwWindowShouldClose(this->handle);
}

void Window::draw() const
{
    glfwSwapBuffers(this->handle);
    glfwPollEvents();
}
