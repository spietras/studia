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

Camera *cameraPtr;      //in order to change camera view we need access

void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode)
{
    float changePosition;
    float speed = 0.25f;
    if (key == GLFW_KEY_UP && action == GLFW_REPEAT) //move camera forward
    {
        changePosition = cameraPtr->getPosition().z;
        changePosition = changePosition - speed;
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition().x, cameraPtr->getPosition().y, changePosition));
    }

    else if (key == GLFW_KEY_UP && action == GLFW_PRESS) //move camera forward
    {
        changePosition = cameraPtr->getPosition().z;
        changePosition = changePosition - speed;
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition().x, cameraPtr->getPosition().y, changePosition));
    }

    if (key == GLFW_KEY_DOWN && action == GLFW_REPEAT) //move camera backward
    {
        changePosition = cameraPtr->getPosition().z;
        changePosition = changePosition + speed;
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition().x, cameraPtr->getPosition().y, changePosition));
    }

    else if (key == GLFW_KEY_DOWN && action == GLFW_PRESS) //move camera backward
    {
        changePosition = cameraPtr->getPosition().z;
        changePosition = changePosition + speed;
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition().x, cameraPtr->getPosition().y, changePosition));
    }

    if (key == GLFW_KEY_RIGHT && action == GLFW_REPEAT) //move camera to the right
    {
        changePosition = cameraPtr->getPosition().x;
        changePosition = changePosition + speed;
        cameraPtr->setPosition(glm::vec3(changePosition, cameraPtr->getPosition().y, cameraPtr->getPosition().z));
    }

    else if (key == GLFW_KEY_RIGHT && action == GLFW_PRESS) //move camera to the right
    {
        changePosition = cameraPtr->getPosition().x;
        changePosition = changePosition + speed;
        cameraPtr->setPosition(glm::vec3(changePosition, cameraPtr->getPosition().y, cameraPtr->getPosition().z));
    }

    if (key == GLFW_KEY_LEFT && action == GLFW_REPEAT) //move camera to the left
    {
        changePosition = cameraPtr->getPosition().x;
        changePosition = changePosition - speed;
        cameraPtr->setPosition(glm::vec3(changePosition, cameraPtr->getPosition().y, cameraPtr->getPosition().z));
    }

    else if (key == GLFW_KEY_LEFT && action == GLFW_PRESS) //move camera to the left
    {
        changePosition = cameraPtr->getPosition().x;
        changePosition = changePosition - speed;
        cameraPtr->setPosition(glm::vec3(changePosition, cameraPtr->getPosition().y, cameraPtr->getPosition().z));
    }
}

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

    const std::string depthVertexShaderPath = "res/shaders/depth.vs";
    const std::string depthFragmentShaderPath = "res/shaders/depth.fs";

    const std::string skyboxVertexShaderPath = "res/shaders/skybox.vs";
    const std::string skyboxFragmentShaderPath = "res/shaders/skybox.fs";

    Window w(SCR_WIDTH, SCR_HEIGHT, TITLE);
    w.setKeyCallback(key_callback);

    Renderer r(BG_COLOR);
    AbsorberShaderProgram asp(absorberVertexShaderPath, absorberFragmentShaderPath);
    LightShaderProgram lsp(lightVertexShaderPath, lightFragmentShaderPath);
    DepthShaderProgram dsp(depthVertexShaderPath, depthFragmentShaderPath);
    SkyboxShaderProgram sbsp(skyboxVertexShaderPath, skyboxFragmentShaderPath);

    Texture skyboxTexture({"res/textures/skybox/ely_nevada/nevada_ft.tga",
                           "res/textures/skybox/ely_nevada/nevada_bk.tga",
                           "res/textures/skybox/ely_nevada/nevada_up.tga",
                           "res/textures/skybox/ely_nevada/nevada_dn.tga",
                           "res/textures/skybox/ely_nevada/nevada_rt.tga",
                           "res/textures/skybox/ely_nevada/nevada_lf.tga"},
                          0);

    Texture woodTexture("res/textures/wood.bmp", Texture::LINEAR);
    Texture groundTexture("res/textures/sand.bmp", Texture::REAPETED);

    Scene s;
    Camera c;
    cameraPtr = &c;

    //create model and two entities based on this model
    CubeModel cm(0.25f, 0, 1, 2);
    CubeModel cm2(0.05f, 0, 1, 2);
    CubeModel ctree(0.1f, 0, 1, 2);

    //materials
    Material m1(ColorInt(48, 98, 114), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material m2(ColorInt(241, 140, 142), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material tree(ColorInt(0, 255, 0), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material msky(ColorInt(255, 255, 255), ColorFloat(0.5f, 0.5f, 0.5f), 0.0f);

    Absorber cube(cm, m1);

    int rw = 5;
    std::vector<Absorber> vector;
    for(int i = 0; i<=rw; i++)
    {
        Absorber atree(ctree, tree);
        vector.push_back(atree);
    }

    Absorber cube2(cm, m1, woodTexture);
    Absorber cube3(cm, m1);
    cube3.scale(10.0f);
    cube3.setPosition({0.0f, -2.0f, 0.0f});

    CubeModel cm3(30.0f, 0, 1, 2);
    Skybox skybox(cm3, msky, skyboxTexture);

    PlaneModel planeM(100.5f, 100.5f, 0, 1, 2);
    Absorber plane(planeM, m2, groundTexture);
    s.addAbsorber(plane);

    s.addAbsorber(cube);
    s.addAbsorber(cube2);
    s.addAbsorber(cube3);
    s.addSkybox(skybox);

    //light
    PointLightAttributes pla(ColorInt(255, 0, 0), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    PointLight light(cm2, pla);
    s.addLight(light);

    light.setPosition({-0.5f, 0.0f, -1.0f});

    //light
    PointLightAttributes pla2(ColorInt(0, 0, 255), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    PointLight light2(cm2, pla2);
    s.addLight(light2);

    light2.setPosition({0.5f, 0.0f, -1.0f});

    DirectionalLightAttributes dla({0.0f, -1.0f, 1.0f}, ColorInt(255, 255, 255), 0.2f, 0.4f, 0.5f);
    DirectionalLight dl(dla);
    s.setDirectionalLight(dl);
    s.turnOnShadows();

    plane.rotate(1.57f, {1.0f, 0.0f, 0.0f});
    plane.setPosition({0.0f, -1.0f, 0.0f});

    float deltaTime;
    float lastFrame = 0.0f;

    float rotationSpeed = 2.0f;
    float circlingSpeed = 2.0f;
    float scalingSpeed = 5.0f;

    int i = 0;
    for(auto el : vector)
    {
        el.setPosition({0.15f*cos(2*3.1416*i/5), 0.01f*sin(2*3.1416*i/5), -2.0f});
        s.addAbsorber(el);
        i++;
    }

    while (!w.shouldClose())
    {
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;


        //apply different transformations 0to entities
        skybox.setPosition({0.0f, 1.0f, 1.0f});
        // skybox.rotate(rotationSpeed * deltaTime, {0.0f, 1.0f, 0.0f});
        cube.rotate(rotationSpeed * deltaTime, {1.0f, -1.0f, 0.0f});
        float circleTheta = currentFrame * circlingSpeed;
        cube.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), 0.0f));

        cube2.rotate(rotationSpeed * deltaTime, {0.0f, 2.0f, 1.0f});

        float scaleDelta = currentFrame * scalingSpeed;
        cube2.setScale({std::sin(scaleDelta) + 1.0f, std::sin(scaleDelta) + 1.0f, std::sin(scaleDelta) + 1.0f});

        //c.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), -3.0f));
        w.draw(r, s, dsp, asp, lsp, sbsp, c);

    }
    return 0;
}