#define GLEW_STATIC

#include <cmath>
#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "entities/models/CubeModel.h"
#include "rendering/Window.h"
#include "rendering/Camera.h"
#include "glm/ext.hpp"
#include "CubeMap.h"

int main()
{
    const unsigned int SCR_WIDTH = 600;
    const unsigned int SCR_HEIGHT = 600;
    const std::string TITLE = "Wiatrak";
    const ColorInt BG_COLOR = {15, 15, 15};

    const std::string absorberVertexShaderPath = "res/shaders/absorber.vs";
    const std::string absorberFragmentShaderPath = "res/shaders/absorber.fs";

    const std::string lightVertexShaderPath = "res/shaders/light.vs";
    const std::string lightFragmentShaderPath = "res/shaders/light.fs";

    const std::string cubeMapVertexShaderPath = "res/shaders/cubemap.vs";
    const std::string cubeMapFragmentShaderPath = "res/shaders/cubemap.fs";

    const std::string skyboxVertexShaderPath = "res/shaders/skybox.vs";
    const std::string skyboxFragmentShaderPath = "res/shaders/skybox.fs";

    Window w(SCR_WIDTH, SCR_HEIGHT, TITLE);

    Renderer r(BG_COLOR);
    AbsorberShaderProgram asp(absorberVertexShaderPath, absorberFragmentShaderPath);
    LightShaderProgram lsp(lightVertexShaderPath, lightFragmentShaderPath);
    ShaderProgram cmsp(cubeMapVertexShaderPath, cubeMapFragmentShaderPath);
    ShaderProgram sbsp(skyboxVertexShaderPath, skyboxFragmentShaderPath);

    cmsp.use();
    cmsp.setUniformInt("skybox", 0);

    sbsp.use();
    sbsp.setUniformInt("skybox", 0);

    CubeMap skyboxMap({"res/textures/skybox/ely_nevada/nevada_rt.tga",
                       "res/textures/skybox/ely_nevada/nevada_lf.tga",
                       "res/textures/skybox/ely_nevada/nevada_up.tga",
                       "res/textures/skybox/ely_nevada/nevada_dn.tga",
                       "res/textures/skybox/ely_nevada/nevada_ft.tga",
                       "res/textures/skybox/ely_nevada/nevada_bk.tga"});
    Scene s;
    Camera c;

    //create model and two entities based on this model
    CubeModel cm(0.25f, 0, 1);
    CubeModel cm2(0.05f, 0, 1);

    //materials
    Material m1(ColorInt(48, 98, 114), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material m2(ColorInt(241, 140, 142), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);

    Absorber cube(cm, m1);
    Absorber cube2(cm, m2);

    CubeModel cm2(2.5f, 0, 1);
    Absorber skybox(cm2, m2);

    s.addAbsorber(cube);
    s.addAbsorber(cube2);
    s.addAbsorber(skybox);

    //light
    PointLightAttributes pla(ColorInt(255, 255, 255), 0.1f, 0.75f, 1.0f, 1.0f, 0.09f, 0.032f);
    PointLight light(cm2, pla);
    s.addLight(light);

    light.setPosition({-0.5f, 0.0f, -1.0f});

    //light
    PointLightAttributes pla2(ColorInt(255, 255, 255), 0.1f, 0.75f, 1.0f, 1.0f, 0.09f, 0.032f);
    PointLight light2(cm2, pla2);
    s.addLight(light2);

    light2.setPosition({0.5f, 0.0f, -1.0f});

    DirectionalLightAttributes dla({0.0f, -1.0f, 0.0f}, ColorInt(0, 255, 0), 0.05f, 0.4f, 0.5f);
    DirectionalLight dl(dla);
    s.setDirectionLight(dl);

    float deltaTime;
    float lastFrame = 0.0f;

    float rotationSpeed = 2.0f;
    float circlingSpeed = 2.0f;
    float scalingSpeed = 5.0f;

    glDisable(GL_CULL_FACE);

    while (!w.shouldClose())
    {
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;

        //apply different transformations to entities
        skybox.setPosition({0.0f, 0.0f, 2.0f});
        cube.rotate(rotationSpeed * deltaTime, {1.0f, -1.0f, 0.0f});
        float circleTheta = currentFrame * circlingSpeed;
        cube.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), 0.0f));

        cube2.rotate(rotationSpeed * deltaTime, {0.0f, 2.0f, 1.0f});

        float scaleDelta = currentFrame * scalingSpeed;
        cube2.setScale({std::sin(scaleDelta) + 1.0f, std::sin(scaleDelta) + 1.0f, std::sin(scaleDelta) + 1.0f});

        c.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), -3.0f));
        c.setViewDirection(glm::vec3(0.0f, 0.0f, 1.0f));
        w.draw(r, s, asp, lsp, c);
    }
    return 0;
}