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
#include "entities/models/Sphere.h"

#define STB_IMAGE_IMPLEMENTATION

Camera *cameraPtr; //in order to change camera view we need access

bool drop_fan = 0;
float rotationSpeed = 20.0f;
DirectionalLight* sunlight;
std::vector<PointLight*> groundLights;
std::vector<PointLight*> windmillLights;

void keyCallback(GLFWwindow *window, int key, int scancode, int action, int mode)
{
    float speed = 0.25f;
    if (key == GLFW_KEY_W && (action == GLFW_REPEAT || action == GLFW_PRESS)) //move camera forward
    {
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition() + cameraPtr->getViewDirection() * speed));
    }

    if (key == GLFW_KEY_S && (action == GLFW_REPEAT || action == GLFW_PRESS)) //move camera backward
    {
        cameraPtr->setPosition(glm::vec3(cameraPtr->getPosition() - cameraPtr->getViewDirection() * speed));
    }

    if (key == GLFW_KEY_D && (action == GLFW_REPEAT || action == GLFW_PRESS)) //move camera to the right
    {
        cameraPtr->setPosition(cameraPtr->getPosition() + glm::normalize(glm::cross(cameraPtr->getViewDirection(), cameraPtr->getUP())) * speed);
    }

    if (key == GLFW_KEY_A && (action == GLFW_REPEAT || action == GLFW_PRESS)) //move camera to the left
    {
        cameraPtr->setPosition(cameraPtr->getPosition() - glm::normalize(glm::cross(cameraPtr->getViewDirection(), cameraPtr->getUP())) * speed);
    }

    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) //close window
        glfwSetWindowShouldClose(window, GL_TRUE);

    if (key == GLFW_KEY_SPACE && action == GLFW_PRESS) //close window
        drop_fan = 1;

    if (key == GLFW_KEY_Q && (action == GLFW_REPEAT || action == GLFW_PRESS)) //close window
        rotationSpeed -= 1;

    if (key == GLFW_KEY_E && (action == GLFW_REPEAT || action == GLFW_PRESS)) //close window
        rotationSpeed += 1;

    float lightChangeSpeed = 0.1f;

    if(key == GLFW_KEY_1 && (action == GLFW_PRESS || action == GLFW_REPEAT))
        sunlight->setIntensity(sunlight->getAttributes().globalIntensity + lightChangeSpeed);

    if(key == GLFW_KEY_2 && (action == GLFW_PRESS || action == GLFW_REPEAT))
        sunlight->setIntensity(sunlight->getAttributes().globalIntensity - lightChangeSpeed);

    if(key == GLFW_KEY_3 && (action == GLFW_PRESS || action == GLFW_REPEAT))
        for(auto groundLight : groundLights)
            groundLight->setIntensity(groundLight->getAttributes().globalIntensity + lightChangeSpeed);

    if(key == GLFW_KEY_4 && (action == GLFW_PRESS || action == GLFW_REPEAT))
        for(auto groundLight : groundLights)
            groundLight->setIntensity(groundLight->getAttributes().globalIntensity - lightChangeSpeed);

    if(key == GLFW_KEY_5 && (action == GLFW_PRESS || action == GLFW_REPEAT))
        for(auto windmillLight : windmillLights)
            windmillLight->setIntensity(windmillLight->getAttributes().globalIntensity + lightChangeSpeed);

    if(key == GLFW_KEY_6 && (action == GLFW_PRESS || action == GLFW_REPEAT))
        for(auto windmillLight : windmillLights)
            windmillLight->setIntensity(windmillLight->getAttributes().globalIntensity - lightChangeSpeed);

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
    CubeModel cm2(0.05f, 0, 1, 2);
    CubeModel ctree(0.05f, 0, 1, 2);
    CubeModel cm3(30.0f, 0, 1, 2);
    PlaneModel planeM(100.0f, 100.0f, 0, 1, 2);

    //materials
    Material m2(ColorInt(241, 140, 142), ColorFloat(0.5f, 0.5f, 0.5f), 4.0f);
    Material tree(ColorInt(0, 255, 0), ColorFloat(0.5f, 0.5f, 0.5f), 32.0f);
    Material msky(ColorInt(255, 255, 255), ColorFloat(0.5f, 0.5f, 0.5f), 0.0f);

    /*  skybox  */
    Skybox skybox(cm3, msky, skyboxTexture);
    skybox.setPosition({0.0f, 1.0f, 1.0f});

    /*  light attributes  */
    PointLightAttributes pla(ColorInt(255, 0, 0), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    PointLightAttributes pla2(ColorInt(0, 0, 255), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    PointLightAttributes pla3(ColorInt(255, 255, 255), 0.2f, 0.75f, 1.0f, 1.0f, 0.22f, 0.2f);
    DirectionalLightAttributes dla({1.0f, -1.0f, 0.0f}, ColorInt(50, 50, 50), 0.2f, 0.4f, 0.5f);

    /* light */
    PointLight light3(cm2, pla3);
    PointLight light(cm2, pla);
    PointLight light2(cm2, pla2);
    DirectionalLight dl(dla);

    light.setPosition({-0.5f, 0.0f, -1.0f});
    light2.setPosition({0.5f, 0.0f, -1.0f});
    light3.setPosition({0.0f, 0.0f, 0.6f});

    sunlight = &dl;
    groundLights.push_back(&light);
    groundLights.push_back(&light2);
    groundLights.push_back(&light3);

    /* absorbers */

    Absorber plane(planeM, m2, groundTexture);
    plane.setPosition({0.0f, 0.0f, 0.0f});

    /* adding to scene */

    s.addLight(light3);
    s.addAbsorber(plane);
    s.addSkybox(skybox);
    s.addLight(light);
    s.addLight(light2);
    s.setDirectionalLight(dl);
    s.turnOnShadows();

    // FOUNDATION VVV
    //height of the windmill
    float root_h = 1.8f;

    //creating root
    Absorber foundation_root(ctree, tree, woodTexture);
    foundation_root.setPosition({0.0f, root_h, 0.0f});
    s.addAbsorber(foundation_root);

    //angle between height and legs (approx)
    double angle_degrees = 6;
    double angle = angle_degrees * 3.141592 / 180;

    //legs info
    float plank_length = (root_h) / cos(angle); //needs to scale with height
    float plank_width = 0.015f;
    float plank_depth = 0.015f;

    //settings for legs
    glm::vec3 leg_rotations[] = {{0.0f, 0.0f, -1.0f}, {1.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 1.0f}, {-1.0f, 0.0f, 0.0f}};
    glm::vec3 leg_positions[] = {{-0.7071 * (plank_length)*sin(angle) / 2, root_h / 2, 0.7071 * (plank_length)*sin(angle) / 2},
                                 {-0.7071 * (plank_length)*sin(angle) / 2, root_h / 2, -0.7071 * (plank_length)*sin(angle) / 2},
                                 {0.7071 * (plank_length)*sin(angle) / 2, root_h / 2, -0.7071 * (plank_length)*sin(angle) / 2},
                                 {0.7071 * (plank_length)*sin(angle) / 2, root_h / 2, 0.7071 * (plank_length)*sin(angle) / 2}};

    //creating legs
    CuboidModel leg(plank_width, plank_length, plank_depth, 0, 1, 2);
    Absorber legs(leg, tree, woodTexture);
    std::vector<Absorber *> legs_v;
    Absorber *obj2[4];
    for (int i = 0; i < 4; i++)
    {
        obj2[i] = new Absorber(leg, tree, woodTexture);
        legs_v.push_back(obj2[i]);
        obj2[i]->rotate(angle, leg_rotations[i]);
        obj2[i]->rotate(0.785, glm::vec3(0.0f, 1.0f, 0.0f));
        s.addAbsorber(*obj2[i]);
        obj2[i]->setPosition({leg_positions[i]});
        foundation_root.addChild(obj2[i]);
    }
    // FOUNDATION ^^^

    // PADDLES VVV
    int no_paddles = 15;

    //how far from the center the middle point of a paddle should be
    float radius_of_paddles = 0.15f;

    //paddle info
    float pad_length = 0.15f;
    float pad_width = 0.03f;
    float pad_depth = 0.01f;

    //connector between the center and a paddle
    float con_length = radius_of_paddles;
    float con_width = pad_width / 2;
    float con_depth = pad_depth / 2;

    CuboidModel child(pad_width, pad_length, pad_depth, 0, 1, 2); //paddle
    CuboidModel connector(con_width, con_length, con_depth, 0, 1, 2); //connector
    Cylinder cylinder(0.025f, 0.025f, 20, 0, 1, 20); //the center

    //creating the center
    Absorber parent(cylinder, tree, woodTexture);
    parent.rotate(3.14 / 2, glm::vec3(1.0f, 0.0f, 0.0f));
    parent.setPosition({0.0f, 0.0f, -2.0f});
    s.addAbsorber(parent);

    //creating paddles and connectors in a loop
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

    //setting and rotating paddles and connectors
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

    //the connector between root(where legs join) and the center of the fan
    float base_pad_connector_length = 0.1f;
    CuboidModel base_pad_con(0.01f, 0.01f, base_pad_connector_length, 0, 1, 2);
    Absorber base_pad_con_abs(base_pad_con, tree, woodTexture);
    s.addAbsorber(base_pad_con_abs);

    //joining connector to root
    base_pad_con_abs.setPosition(foundation_root.getPosition());
    base_pad_con_abs.setPosition(foundation_root.getPosition() + (glm::vec3{0.0f, 0.0f, base_pad_connector_length / 2}));
    foundation_root.addChild(&base_pad_con_abs);

    //joining the center to root
    parent.setPosition(foundation_root.getPosition());
    parent.setPosition(parent.getPosition() + (glm::vec3{0.0f, 0.0f, base_pad_connector_length}));
    foundation_root.addChild(&parent);

    //light in the middle od the fan
    CubeModel small_light(0.01f, 0, 1, 2);
    PointLight light4(small_light, pla3);
    s.addLight(light4);
    light4.setPosition(parent.getPosition());
    parent.addChild(&light4);
    // PADDLES ^^^

    windmillLights.push_back(&light4);

    // PADDLES ^^^^

    // TAIL VVV
        //the wide tail
    float cm_tail_length = 0.17f;
    CuboidModel cm_tail(cm_tail_length, 0.10f, 0.005f, 0, 1, 2);
    Absorber tail(cm_tail, tree, woodTexture);
    s.addAbsorber(tail);

        //the thin connector appended to tail
    float con_tail_length = 0.2f;
    CuboidModel cm_tb_con(con_tail_length, 0.01f, 0.005f, 0, 1, 2);
    Absorber tail_base_connector(cm_tb_con, tree, woodTexture);
    s.addAbsorber(tail_base_connector);

        //the other end of the connector
    CuboidModel cm_tail_end(0.01f, 0.01f, 0.01f, 0, 1, 2);
    Absorber tail_end(cm_tail_end, tree, woodTexture);
    s.addAbsorber(tail_end);

        //making relations between the objects
    tail_end.translate({-(con_tail_length) / 2, 0.0f, 0.0f});
    tail_end.addChild(&tail_base_connector);
    tail_end.setPosition(tail_end.getPosition() + glm::vec3(-(cm_tail_length + con_tail_length) / 2, 0.0f, 0.0f));
    tail_base_connector.addChild(&tail);
    tail_end.setPosition(foundation_root.getPosition() - glm::vec3(0.0f, 0.0f, 0.0f));

    foundation_root.addChild(&tail_end);
    // TAIL ^^^

    // BARRELS VVV
    float barrel_h = 0.105;
    float barrel_r = 0.025;
    Cylinder barrel_c(barrel_r, barrel_h, 20, 0, 1, 20);

    Absorber barrel1(barrel_c, tree, woodTexture);
    s.addAbsorber(barrel1);
    barrel1.setPosition({-((plank_length)*sin(angle) * 1.4 + barrel_r * 2), barrel_h / 2, 0.0f});
    foundation_root.addChild(&barrel1);

    Absorber barrel2(barrel_c, tree, woodTexture);
    s.addAbsorber(barrel2);
    barrel2.setPosition({(plank_length)*sin(angle) * 1.4 + barrel_r * 2, barrel_h / 2, (plank_length)*sin(angle) * 1.6 + barrel_r * 2});
    foundation_root.addChild(&barrel2);

    Absorber barrel3(barrel_c, tree, woodTexture);
    s.addAbsorber(barrel3);
    barrel3.rotate(3.14 / 2, glm::vec3(1.0f, 0.0f, 0.0f));
    barrel3.setPosition({-((plank_length)*sin(angle) * 1.3 + barrel_r * 2), barrel_h / 2, (plank_length)*sin(angle) * 2 + barrel_r * 2});
    barrel3.translate({0.0f, -barrel_h / 2 + barrel_r, 0.0f});
    foundation_root.addChild(&barrel3);
    // BARRELS ^^^

    //initial positions of objects
    tail_end.rotateRelative(foundation_root.getPosition(), 0.2, glm::vec3(0.0f, 0.0f, 1.0f));
    tail_end.rotateRelative(foundation_root.getPosition(), 3.14 / 3, glm::vec3(0.0f, 1.0f, 0.0f));
    foundation_root.setPosition({1.0f, root_h, 0.0f});
    //foundation_root.rotate(0.1f, {0.0f, 1.0f, 0.0f});
    foundation_root.rotate(-M_PI / 4.0f, {0.0f, 1.0f, 0.0f});

    //moving tail paramteres
    int tail_direction = 1;
    float time_until_switch = 1.2;
    float time_counter = time_until_switch;

    //loop
    float deltaTime;
    float lastFrame = 0.0f;
    while (!w.shouldClose())
    {
        // time calculations
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;

        float paddles_direction = -1; // controls the directon of moving paddles and of the fallen fan
 
        //parameters for realistic fall physics
        float fall_speed = 0.0; 
        float fall_acceleration = 1.2;

        if (drop_fan)
        {
            fall_speed += deltaTime*fall_acceleration; //acceleration during fall

            //fall animation
            if (parent.getPosition().y >= radius_of_paddles + pad_length / 2)
                parent.translate({0.0f, -fall_speed, 0.0f});

            //movement sideways during and after fall
            parent.translate(foundation_root.getRotation() * glm::vec3(-0.13 * (radius_of_paddles+pad_length/2) * rotationSpeed * deltaTime * paddles_direction, 0.0f, 0.0f));
        }

        //rotating tail
        time_counter -= deltaTime;
        if (time_counter < 0)
        {
            time_counter = time_until_switch;
            tail_direction *= -1;
        }
        tail_end.rotateRelative(foundation_root.getPosition(), tail_direction * rotationSpeed * deltaTime * 0.03, glm::vec3(0.0f, 1.0f, 0.0f));
        //rotating fans
        parent.rotateLocal(rotationSpeed * deltaTime * 0.1 * paddles_direction, {0.0f, 1.0f, 0.0f}); 

        w.draw(r, s, dsp, asp, lsp, sbsp, c);
    }
    return 0;
}