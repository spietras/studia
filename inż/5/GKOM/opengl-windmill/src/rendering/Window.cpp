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
    glEnable(GL_CULL_FACE); //enable face culling (but only to get rid of peter panning)
    glCullFace(GL_BACK);
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

void Window::draw(const Renderer &renderer, const Scene &scene,
                  const DepthShaderProgram &depthShaderProgram,
                  const AbsorberShaderProgram &absorberShaderProgram,
                  const LightShaderProgram &lightShaderProgram, const Camera &camera)
{
    if(scene.isShadowsTurnedOn())
    {
        const DirectionalLight *light = scene.getDirectionalLight();
        const int currentWidth = width;
        const int currentHeight = height;

        //resize viewport to match depth map size
        resize(light->getShadowWidth(), light->getShadowHeight());
        renderer.renderShadowMap(scene, depthShaderProgram);

        //resize back
        resize(currentWidth, currentHeight);
    }

    renderer.render(scene, camera, absorberShaderProgram, lightShaderProgram);

    glfwSwapBuffers(this->handle); //swap front and back buffer, because we use double buffering
    glfwPollEvents(); //process all events on windows in this frame
}

void Window::makeContextCurrent() const
{
    glfwMakeContextCurrent(this->handle);
}

void Window::setKeyCallback(void (*f)(GLFWwindow*, int, int, int, int)) const
{
    glfwSetKeyCallback(this->handle, *f);
}
