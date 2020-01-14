#include <cmath>

#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "entities/models/CubeModel.h"
#include "utils/Window.h"
#include <iostream>

int main()
{
    const unsigned int SCR_WIDTH = 600;
    const unsigned int SCR_HEIGHT = 600;
    const std::string TITLE = "Wiatrak";
    const ColorFloat BG_COLOR = {0.2f, 0.3f, 0.3f, 1.0f};

    const char *vertexShaderSource = "#version 330 core\n"
                                     "layout (location = 0) in vec3 aPos;\n"
                                     "layout (location = 1) in vec3 aNorm;\n"
                                     "uniform mat4 model;\n"
                                     "out vec3 normal;\n"
                                     "void main()\n"
                                     "{\n"
                                     "  gl_Position = model * vec4(aPos, 1.0);\n"
                                     "  normal = aNorm;\n"
                                     "}\0";
    const char *fragmentShaderSource = "#version 330 core\n"
                                       "in vec3 normal;\n"
                                       "out vec4 FragColor;\n"
                                       "void main()\n"
                                       "{\n"
                                       "   FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n"
                                       "}\n\0";

    Window w(SCR_WIDTH, SCR_HEIGHT, TITLE);

    Renderer r(BG_COLOR);
    ShaderProgram sp(vertexShaderSource, fragmentShaderSource);

    Scene s;

    //create model and two entities based on this model
    CubeModel cm(0.25f, 0, 1);
    Entity cube(cm);
    Entity cube2(cm);
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