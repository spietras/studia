#define GLEW_STATIC

#include <cmath>

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "entities/models/CubeModel.h"
#include "rendering/Window.h"

int main()
{
    const unsigned int SCR_WIDTH = 600;
    const unsigned int SCR_HEIGHT = 600;
    const std::string TITLE = "Wiatrak";
    const ColorInt BG_COLOR = {236, 237, 237};

    const std::string absorberVertexShaderPath = "res/shaders/absorber.vs";
    const std::string absorberFragmentShaderPath = "res/shaders/absorber.fs";

    const std::string lightVertexShaderPath = "res/shaders/light.vs";
    const std::string lightFragmentShaderPath = "res/shaders/light.fs";

    Window w(SCR_WIDTH, SCR_HEIGHT, TITLE);

    Renderer r(BG_COLOR);
    AbsorberShaderProgram asp(absorberVertexShaderPath, absorberFragmentShaderPath);
    LightShaderProgram lsp(lightVertexShaderPath, lightFragmentShaderPath);

    Scene s;

    //create model and two entities based on this model
    CubeModel cm(0.25f, 0, 1);
    Absorber cube(cm, ColorInt(48, 96, 114));
    Absorber cube2(cm, ColorInt(241, 140, 142));
    s.addAbsorber(cube);
    s.addAbsorber(cube2);

    PointLight light(cm, ColorFloat(1.0f, 1.0f, 1.0f));
    s.addLight(light);

    light.setPosition({0.8f, 0.8f, 0.0f});

    float deltaTime;
    float lastFrame = 0.0f;

    float rotationSpeed = 2.0f;
    float circlingSpeed = 2.0f;

    while (!w.shouldClose())
    {
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;

        //apply different transformations to entities

        cube.rotate(rotationSpeed * deltaTime, {1.0f, -1.0f, 0.0f});
        light.rotate(rotationSpeed * deltaTime, {-1.0f, 2.0f, -3.0f});
        float circleTheta = currentFrame * circlingSpeed;
        cube.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), 0.0f));

        cube2.rotate(rotationSpeed * deltaTime, {0.0f, 3.0f, 1.0f});

        w.draw(r, s, asp, lsp);
    }

    return 0;
}