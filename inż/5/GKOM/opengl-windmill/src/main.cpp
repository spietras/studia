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
#include "entities/models/Pyramid.h"
#include "entities/models/RegularPyramid.h"
#include "entities/models/CircularFrustum.h"
#include "entities/models/Cylinder.h"
#include "entities/models/Cone.h"
#include "entities/models/Tetrahedron.h"
#include "entities/models/RegularTetrahedron.h"

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
    CubeModel ctree(0.05f, 0, 1, 2);
    CubeModel cm3(30.0f, 0, 1, 2);
    PlaneModel planeM(100.0f, 100.0f, 0, 1, 2);
    RegularTetrahedron tm(2.0f, 0, 1, 2);
    CubeModel small_light(0.01f, 0, 1, 2);

    //materials
    Material m2(ColorInt(241, 140, 142), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material tree(ColorInt(0, 255, 0), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material msky(ColorInt(255, 255, 255), ColorFloat(0.5f, 0.5f, 0.5f), 0.0f);

    /*  skybox  */
    Skybox skybox(cm3, msky, skyboxTexture);
    skybox.setPosition({0.0f, 1.0f, 1.0f});

    /*  light attributes  */
    PointLightAttributes pla(ColorInt(255, 0, 0), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    PointLightAttributes pla2(ColorInt(0, 0, 255), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    PointLightAttributes pla3(ColorInt(255, 255, 255), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    DirectionalLightAttributes dla({1.0f, -2.0f, 0.0f}, ColorInt(50, 50, 50), 0.2f, 0.4f, 0.5f);

    /* light */
    PointLight light3(cm2, pla3);
    PointLight light(cm2, pla);
    PointLight light2(cm2, pla2);
    DirectionalLight dl(dla);


    light.setPosition({-0.5f, 0.0f, -1.0f});
    light2.setPosition({0.5f, 0.0f, -1.0f});
    light3.setPosition({0.0f, 0.0f, 0.6f});

    /* absorbers */

    Absorber plane(planeM, m2, groundTexture);
    plane.rotate(1.57f, {1.0f, 0.0f, 0.0f});
    plane.setPosition({0.0f, 0.0f, 0.0f});

    /* adding to scene */

    s.addLight(light3);
    s.addAbsorber(plane);
    s.addSkybox(skybox);
    s.addLight(light);
    s.addLight(light2);
    s.setDirectionalLight(dl);
    s.turnOnShadows();

    /*  animation params  */
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
    Cylinder cylinder(0.025f, 0.025f, 20, 0, 1, 20);
    Absorber parent(cylinder, tree, woodTexture);
    parent.rotate(3.14/2, glm::vec3(1.0f, 0.0f, 0.0f));
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

    // TAIL VVV

    float cm_tail_length = 0.17f;
    CuboidModel cm_tail(cm_tail_length, 0.10f, 0.005f, 0, 1, 2);
    Absorber tail(cm_tail, tree, woodTexture);
    s.addAbsorber(tail);

    float con_tail_length = 0.2f;
    CuboidModel cm_tb_con(con_tail_length, 0.01f, 0.005f, 0, 1, 2);
    Absorber tail_base_connector(cm_tb_con, tree, woodTexture);
    s.addAbsorber(tail_base_connector);

    tail_base_connector.translate({-(cm_tail_length+con_tail_length)/2, 0.0f, 0.0f});
    tail_base_connector.addChild(&tail);
    tail_base_connector.setPosition(foundation_root.getPosition()-glm::vec3(-con_tail_length/2, 0.0f, 0.0f));

    foundation_root.addChild(&tail_base_connector);

    // TAIL ^^^

    tail_base_connector.rotate(20, glm::vec3(0.0f, 0.0f, 1.0f));

    
    while (!w.shouldClose())
    {

        // DirectionalLightAttributes dla({0.0f, -1.0f, 1.0f}, ColorInt(255, 255, 255), 0.2f, 0.4f, 0.5f);
        // DirectionalLight dl(dla);
        // s.setDirectionalLight(dl);
        
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;

        foundation_root.setPosition({1.0f, root_h, 0.0f});
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

        /*  rendering  */

        //c.setPosition(glm::vec3(0.5f * std::cos(circleTheta), 0.5f * std::sin(circleTheta), -3.0f));
    
        w.draw(r, s, dsp, asp, lsp, sbsp, c);
    }
    return 0;
}