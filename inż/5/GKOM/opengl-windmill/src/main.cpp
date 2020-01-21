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

Camera *cameraPtr; //in order to change camera view we need access

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
    w.makeContextCurrent();
    w.setKeyCallback(keyCallback);
    w.setCursorCallback(cursorCallback);  //if you want to use mouse to rotate unable this line

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
    Texture woodTextureR("res/textures/wood.bmp", Texture::REAPETED);
    Texture groundTexture("res/textures/sand.bmp", Texture::REAPETED);

    Scene s;
    Camera c;
    cameraPtr = &c;

    //create model and two entities based on this model
    CubeModel cm2(0.05f, 0, 1, 2);
    CubeModel ctree(0.05f, 0, 1, 2);
    CubeModel small_light(0.01f, 0, 1, 2);

    //materials
    Material m2(ColorInt(241, 140, 142), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material tree(ColorInt(0, 255, 0), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material tree2(ColorInt(255, 255, 0), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material msky(ColorInt(255, 255, 255), ColorFloat(0.5f, 0.5f, 0.5f), 0.0f);

    CubeModel cm3(30.0f, 0, 1, 2);
    Skybox skybox(cm3, msky, skyboxTexture);

    PlaneModel planeM(100.5f, 100.5f, 0, 1, 2);
    Absorber plane(planeM, m2, groundTexture);
    s.addAbsorber(plane);
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

    //light
    PointLightAttributes pla3(ColorInt(255, 255, 255), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    PointLight light3(cm2, pla3);
    s.addLight(light3);

    light3.setPosition({0.0f, 0.0f, 0.6f});

    DirectionalLightAttributes dla({0.0f, -1.0f, 1.0f}, ColorInt(50, 50, 50), 0.2f, 0.4f, 0.5f);
    DirectionalLight dl(dla);
    s.setDirectionalLight(dl);
    s.turnOnShadows();

    plane.rotate(1.57f, {1.0f, 0.0f, 0.0f});
    plane.setPosition({0.0f, 0.0f, 0.0f});

    float deltaTime;
    float lastFrame = 0.0f;

    float rotationSpeed = 2.0f;
    float circlingSpeed = 2.0f;
    float scalingSpeed = 5.0f;

    // FOUNDATION VVV
    float root_h = 0.7f;
    float root_lr = 0.5f;
    float root_fb = 0.0f;


    Absorber foundation_root(ctree, tree, woodTexture);
    foundation_root.setPosition({0.0f, root_h, 0.0f});
    s.addAbsorber(foundation_root);

    double angle_degrees = 6;
    double angle = angle_degrees * 3.141592 / 180;

    float plank_length = (root_h) / cos(angle);
    float plank_width = 0.015f;
    float plank_depth = 0.015f;

    CuboidModel leg(plank_width, plank_length, plank_depth, 0, 1, 2);
    Absorber legs(leg, tree, woodTexture);
    std::vector<Absorber *> legs_v;
    int no_of_legs = 4;
    Absorber *obj2[4];
    glm::vec3 leg_rotations[] = {{0.0f, 0.0f, -1.0f}, {1.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 1.0f}, {-1.0f, 0.0f, 0.0f}};
    glm::vec3 leg_positions[] = {{-(plank_length)*sin(angle) / 2, root_h / 2, (plank_length)*sin(angle) / 2},
                                 {-(plank_length)*sin(angle) / 2, root_h / 2, -(plank_length)*sin(angle) / 2},
                                 {(plank_length)*sin(angle) / 2, root_h / 2, -(plank_length)*sin(angle) / 2},
                                 {(plank_length)*sin(angle) / 2, root_h / 2, (plank_length)*sin(angle) / 2}};
    for (int i = 0; i < no_of_legs; i++)
    {
        obj2[i] = new Absorber(leg, tree, woodTexture);
        legs_v.push_back(obj2[i]);
        obj2[i]->rotate(angle, leg_rotations[i]);
        obj2[i]->rotate(0.785, glm::vec3(0.0f, 1.0f, 0.0f));
        s.addAbsorber(*obj2[i]);
        obj2[i]->setPosition({leg_positions[i]});
        foundation_root.addChild(obj2[i]);
    }

        foundation_root.setPosition({root_lr, 0.5f, root_fb});


    // FOUNDATION ^^^

    // PADDLES VVVVV

    float radius_of_paddles = 0.15f;
    float pad_length = 0.15f;
    float pad_width = 0.03f;
    float pad_depth = 0.01f;

    float con_length = radius_of_paddles;
    float con_width = pad_width/2;
    float con_depth = pad_depth/2;

    CuboidModel child(pad_width, pad_length, pad_depth, 0, 1, 2);
    CuboidModel connector(con_width, con_length, con_depth, 0, 1, 2);
    Absorber parent(ctree, tree, woodTexture);
    parent.setPosition({0.0f, 0.0f, -2.0f});
    s.addAbsorber(parent);
    int no_paddles = 15;
    std::vector<Absorber *> vec_paddles;
    std::vector<Absorber *> vec_connectors;
    Absorber *obj[100];
    Absorber *con[100];
    for (int i = 0; i < no_paddles; i++)
    {
        obj[i] = new Absorber(child, tree, woodTexture);
        con[i] = new Absorber(connector, tree, woodTexture);
        vec_paddles.push_back(obj[i]);
        vec_connectors.push_back(con[i]);
        parent.addChild(obj[i]);
        parent.addChild(con[i]);
    }
    double i = 0;
    for (auto el : vec_paddles)
    {
        el->setPosition({radius_of_paddles * sin(2 * 3.1416 * (i) / (no_paddles)), radius_of_paddles * cos(2 * 3.1416 * (i) / no_paddles), -2.0f});
        el->rotate(2 * 3.1416 * i / double(no_paddles) + 3.14, {0.0f, 0.0f, -1.0f});
        s.addAbsorber(*el);
        i++;
    }

    parent.setPosition(foundation_root.getPosition());
    parent.setPosition(parent.getPosition()+(glm::vec3{0.0f, 0.0f, 0.1f}));
    foundation_root.addChild(&parent);

    for (auto el : vec_connectors)
    {
        el->setPosition({radius_of_paddles * 0.5 * sin(2 * 3.1416 * (i) / (no_paddles)), radius_of_paddles * 0.5 * cos(2 * 3.1416 * (i) / no_paddles), -2.0f});
        el->rotate(2 * 3.1416 * i / double(no_paddles) + 3.14, {0.0f, 0.0f, -1.0f});
        s.addAbsorber(*el);
        i++;
    }

    float base_pad_connector_length = 0.1f;
    CuboidModel base_pad_con(0.01f, 0.01f, base_pad_connector_length, 0, 1, 2);
    Absorber base_pad_con_abs(base_pad_con, tree, woodTexture);
    s.addAbsorber(base_pad_con_abs);
    base_pad_con_abs.setPosition(foundation_root.getPosition());
    base_pad_con_abs.setPosition(foundation_root.getPosition()+(glm::vec3{0.0f, 0.0f, base_pad_connector_length/2}));
    foundation_root.addChild(&base_pad_con_abs);

    parent.setPosition(foundation_root.getPosition());
    parent.setPosition(parent.getPosition()+(glm::vec3{0.0f, 0.0f, base_pad_connector_length}));
    foundation_root.addChild(&parent);

    //light
    PointLight light4(small_light, pla3);
    s.addLight(light4);
    light4.setPosition(parent.getPosition());
    parent.addChild(&light4);

    // PADDLES ^^^^
    
    while (!w.shouldClose())
    {

        // DirectionalLightAttributes dla({0.0f, -1.0f, 1.0f}, ColorInt(255, 255, 255), 0.2f, 0.4f, 0.5f);
        // DirectionalLight dl(dla);
        // s.setDirectionalLight(dl);
        
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;

        foundation_root.setPosition({0.0f, root_h, 0.0f});
        parent.rotate(rotationSpeed * deltaTime * 0.1, {0.0f, 0.0f, 1.0f});

        //apply different transformations to entities
        skybox.setPosition({0.0f, 1.0f, 1.0f});
        /*
        // skybox.rotate(rotationSpeed * deltaTime, {0.0f, 1.0f, 0.0f});
        cube.rotate(rotationSpeed * deltaTime, {1.0f, -1.0f, 0.0f});
        float circleTheta = currentFrame * circlingSpeed;
        cube.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), 0.0f));

        cube2.rotate(rotationSpeed * deltaTime, {0.0f, 2.0f, 1.0f});

        float scaleDelta = currentFrame * scalingSpeed;
        cube2.setScale({std::sin(scaleDelta) + 1.0f, std::sin(scaleDelta) + 1.0f, std::sin(scaleDelta) + 1.0f});

        //c.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), -3.0f));
        */
        w.draw(r, s, dsp, asp, lsp, sbsp, c);
    }
    return 0;
}