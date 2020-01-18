#include "Window.h"

Window::Window(int width, int height, const std::string &title) : width(width), height(height), title(title)
{
    //openGL init magic

    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, GLFW_VERSION.MAJOR);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, GLFW_VERSION.MINOR);

    this->handle = glfwCreateWindow(width, height, title.c_str(), nullptr, nullptr);

    glfwMakeContextCurrent(this->handle);

    glewExperimental = GL_TRUE;
    glewInit();

    glViewport(0, 0, width, height);

    glEnable(GL_DEPTH_TEST);
}

Window::~Window()
{
    glfwTerminate();
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

void Window::draw(const Renderer &renderer, const Scene &scene, const AbsorberShaderProgram &absorberShaderProgram,
                  const LightShaderProgram &lightShaderProgram, const Camera &camera) const
{
    renderer.render(scene, camera, absorberShaderProgram, lightShaderProgram);
    glfwSwapBuffers(this->handle); //swap front and back buffer, because we use double buffering
    glfwPollEvents(); //process all events on windows in this frame
}