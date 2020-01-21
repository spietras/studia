#define GLEW_STATIC

#include <cmath>
#include "GL/glew.h"
#include "GLFW/glfw3.h"
#include "entities/models/CubeModel.h"
#include "rendering/Window.h"
#include "rendering/Camera.h"
#include "glm/ext.hpp"
#include "rendering/shaders/SkyboxShaderProgram.h"
#include "utils/Texture.h"
#include "entities/models/PlaneModel.h"

#define STB_IMAGE_IMPLEMENTATION

Camera* cameraPtr;      //in order to change camera view we need access

void keyCallback(GLFWwindow *window, int key, int scancode, int action, int mode)
{
    float speed = 0.25f;
    if (key == GLFW_KEY_UP && action == GLFW_REPEAT) //move camera forward
    {
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition()+cameraPtr->getViewDirection()*speed));
    }

    else if (key == GLFW_KEY_UP && action == GLFW_PRESS) //move camera forward
    {
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition()+cameraPtr->getViewDirection()*speed));
    }

    if (key == GLFW_KEY_DOWN && action == GLFW_REPEAT) //move camera backward
    {
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition()-cameraPtr->getViewDirection()*speed));
    }

    else if (key == GLFW_KEY_DOWN && action == GLFW_PRESS) //move camera backward
    {
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition()-cameraPtr->getViewDirection()*speed));
    }

    if (key == GLFW_KEY_RIGHT && action == GLFW_REPEAT) //move camera to the right
    {
        //cameraPos += glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed;
        cameraPtr->setPosition(cameraPtr->getPosition()+glm::normalize(glm::cross(cameraPtr->getViewDirection(), cameraPtr->getUP())) * speed);
    }

    else if (key == GLFW_KEY_RIGHT && action == GLFW_PRESS) //move camera to the right
    {
        cameraPtr->setPosition(cameraPtr->getPosition()+glm::normalize(glm::cross(cameraPtr->getViewDirection(), cameraPtr->getUP())) * speed);
    }

    if (key == GLFW_KEY_LEFT && action == GLFW_REPEAT) //move camera to the left
    {
        cameraPtr->setPosition(cameraPtr->getPosition()-glm::normalize(glm::cross(cameraPtr->getViewDirection(), cameraPtr->getUP())) * speed);
    }

    else if (key == GLFW_KEY_LEFT && action == GLFW_PRESS) //move camera to the left
    {
        cameraPtr->setPosition(cameraPtr->getPosition()-glm::normalize(glm::cross(cameraPtr->getViewDirection(), cameraPtr->getUP())) * speed);
    }

    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) //close window
        glfwSetWindowShouldClose(window, GL_TRUE);
}

void cursorCallback(GLFWwindow *window, double xPosition, double yPosition)
{
    glm::vec2 newMousePosition = glm::vec2(xPosition, yPosition);
    cameraPtr->updateMouse(newMousePosition);
}

int main()
{
    /*  params  */

    const unsigned int SCR_WIDTH = 600;
    const unsigned int SCR_HEIGHT = 600;
    const std::string TITLE = "Wiatrak";
    const ColorInt BG_COLOR = {15, 15, 15};

    const std::string absorberVertexShaderPath = "res/shaders/absorber.vs";
    const std::string absorberFragmentShaderPath = "res/shaders/absorber.fs";

    const std::string lightVertexShaderPath = "res/shaders/light.vs";
    const std::string lightFragmentShaderPath = "res/shaders/light.fs";

    const std::string depthVertexShaderPath = "res/shaders/depth.vs";
    const std::string depthFragmentShaderPath = "res/shaders/depth.fs";

    const std::string skyboxVertexShaderPath = "res/shaders/skybox.vs";
    const std::string skyboxFragmentShaderPath = "res/shaders/skybox.fs";

    /*  rendering  */

    Window w(SCR_WIDTH, SCR_HEIGHT, TITLE);
    w.makeContextCurrent();
    w.setKeyCallback(keyCallback);
    w.setCursorCallback(cursorCallback);
    
    Renderer r(BG_COLOR);
    AbsorberShaderProgram asp(absorberVertexShaderPath, absorberFragmentShaderPath);
    LightShaderProgram lsp(lightVertexShaderPath, lightFragmentShaderPath);
    DepthShaderProgram dsp(depthVertexShaderPath, depthFragmentShaderPath);
    SkyboxShaderProgram sbsp(skyboxVertexShaderPath, skyboxFragmentShaderPath);

    /*  textures  */

    Texture skyboxTexture({"res/textures/skybox/ely_nevada/nevada_ft.tga",
                           "res/textures/skybox/ely_nevada/nevada_bk.tga",
                           "res/textures/skybox/ely_nevada/nevada_up.tga",
                           "res/textures/skybox/ely_nevada/nevada_dn.tga",
                           "res/textures/skybox/ely_nevada/nevada_rt.tga",
                           "res/textures/skybox/ely_nevada/nevada_lf.tga"});

    Texture woodTexture("res/textures/wood.bmp");
    Texture groundTexture("res/textures/sand.bmp");

    /*  stuff  */

    Scene s;
    Camera c;
    cameraPtr = &c;

    /*  models  */

    CubeModel cm(0.25f, 0, 1, 2);
    CubeModel cm2(0.05f, 0, 1, 2);
    CubeModel ctree(0.15f, 0, 1, 2);
    CubeModel cm3(30.0f, 0, 1, 2);
    PlaneModel planeM(100.0f, 100.0f, 0, 1, 2);

    /*  materials  */

    Material m1(ColorInt(48, 98, 114), ColorFloat(0.5f, 0.5f, 0.5f), 8.0f);
    Material m2(ColorInt(241, 140, 142), ColorFloat(0.5f, 0.5f, 0.5f), 8.0f);
    Material tree(ColorInt(0, 255, 0), ColorFloat(0.5f, 0.5f, 0.5f), 8.0f);
    Material msky(ColorInt(255, 255, 255), ColorFloat(0.5f, 0.5f, 0.5f), 0.0f);

    /*  absorbers  */

    Absorber cube(cm, m1);
    Absorber atree(ctree, tree);
    Absorber cube2(cm, m1, woodTexture);
    Absorber cube3(cm, m1);
    Absorber plane(planeM, m1, groundTexture);

    atree.setPosition({0.0f, 0.0f, -1.5f});
    cube3.scale(10.0f);
    cube3.setPosition({0.0f, -2.0f, 0.0f});
    plane.setPosition({0.0f, -1.0f, 0.0f});

    /*  skybox  */

    Skybox skybox(cm3, msky, skyboxTexture);

    skybox.setPosition({0.0f, 1.0f, 1.0f});

    /*  light attributes  */

    PointLightAttributes pla(ColorInt(255, 0, 0), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    PointLightAttributes pla2(ColorInt(0, 0, 255), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    DirectionalLightAttributes dla({1.0f, -2.0f, 0.0f}, ColorInt(255, 255, 255), 0.2f, 0.4f, 0.5f);

    /*  lights  */

    PointLight light(cm2, pla);
    PointLight light2(cm2, pla2);
    DirectionalLight dl(dla);

    light.setPosition({-0.5f, 0.0f, -1.0f});
    light2.setPosition({0.5f, 0.0f, -1.0f});

    /*  adding to scene  */

    s.addAbsorber(plane);
    s.addAbsorber(cube);
    s.addAbsorber(atree);
    s.addAbsorber(cube2);
    s.addAbsorber(cube3);
    s.addSkybox(skybox);
    s.addLight(light);
    s.addLight(light2);
    s.setDirectionalLight(dl);

    s.turnOnShadows();

    /*  animation params  */

    float deltaTime;
    float lastFrame = 0.0f;

    float rotationSpeed = 2.0f;
    float circlingSpeed = 2.0f;
    float scalingSpeed = 5.0f;
    float upSpeed = 0.001f;

    /*  loop  */

    while (!w.shouldClose())
    {
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;

        /*  animation  */

        cube.rotate(rotationSpeed * deltaTime, {1.0f, -1.0f, 0.0f});
        float circleTheta = currentFrame * circlingSpeed;
        cube.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), 0.0f));

        cube2.rotate(rotationSpeed * deltaTime, {0.0f, 2.0f, 1.0f});

        float scaleDelta = currentFrame * scalingSpeed;
        cube2.setScale({std::sin(scaleDelta) + 1.0f, std::sin(scaleDelta) + 1.0f, std::sin(scaleDelta) + 1.0f});

        /*  rendering  */

        w.draw(r, s, dsp, asp, lsp, sbsp, c);

    }
    return 0;
}