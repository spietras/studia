#include <cmath>

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "entities/models/CubeModel.h"
#include "utils/Window.h"

int main()
{
    const unsigned int SCR_WIDTH = 600;
    const unsigned int SCR_HEIGHT = 600;
    const std::string TITLE = "Wiatrak";
    const ColorInt BG_COLOR = {236, 237, 237};

    const std::string vertexShaderPath = "res/shaders/basic.vs";
    const std::string fragmentShaderPath = "res/shaders/basic.fs";

    Window w(SCR_WIDTH, SCR_HEIGHT, TITLE);

    Renderer r(BG_COLOR);
    ShaderProgram sp(vertexShaderPath, fragmentShaderPath);

    Scene s;

    //create model and two entities based on this model
    CubeModel cm(0.25f, 0, 1);
    Entity cube(cm, ColorInt(48, 95, 114));
    Entity cube2(cm, ColorInt(241, 140, 142));
    s.addEntity(cube);
    s.addEntity(cube2);

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

        cube.rotate(rotationSpeed * deltaTime, glm::vec3(1.0f, -1.0f, 0.0f));
        float circleTheta = currentFrame * circlingSpeed;
        cube.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), 0.0f));

        cube2.rotate(rotationSpeed * deltaTime, glm::vec3(0.0f, 3.0f, 1.0f));

        w.draw(r, s, sp);
    }

    return 0;
}