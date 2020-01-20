#include "Renderer.h"

void Renderer::drawBackground() const
{
    glClearColor(backgroundColor.red,
                 backgroundColor.green,
                 backgroundColor.blue,
                 backgroundColor.alpha);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void Renderer::drawEntity(const Entity &entity) const
{
    const BaseObjectModel &model = entity.getModel();
    glBindVertexArray(model.getVAO());                      //bind model VAO
    glDrawArrays(GL_TRIANGLES, 0, model.getVerticesSize()); //draw
    glBindVertexArray(0);
}

void Renderer::drawSceneAbsorbers(const Scene &scene, const Camera &camera, const AbsorberShaderProgram &shaderProgram) const
{
    const std::vector<const PointLight *> &lights = scene.getLights();
    shaderProgram.setLightsNumber(lights.size());

    const DirectionalLight *directionalLight = scene.getDirectionalLight();
    if (directionalLight != nullptr)
        shaderProgram.setDirectionlight(*directionalLight);

    for (const Absorber *absorber : scene.getAbsorbers())
    {
        //set shader uniforms
        shaderProgram.applyEntityTransformation(*absorber);
        shaderProgram.setAbsorberMaterial(*absorber);
        shaderProgram.setViewPosition(camera.getPosition());
        shaderProgram.setMode(*absorber);

        for (int i = 0; i < lights.size(); i++)
            shaderProgram.setLight(*lights[i], i);

        if (absorber->getMode() == Absorber::TEXTURE)
        {
            glEnable(GL_TEXTURE_2D);
            glActiveTexture(GL_TEXTURE0);
            shaderProgram.setTextureMode(*absorber);
            glBindTexture(GL_TEXTURE_2D, absorber->getTexture().getTextureID());
        }
        drawEntity(*absorber);
    }
}

void Renderer::drawSceneLights(const Scene &scene, const LightShaderProgram &shaderProgram) const
{
    for (const PointLight *light : scene.getLights())
    {
        //set shader uniforms
        shaderProgram.applyEntityTransformation(*light);
        shaderProgram.setLightColor(*light);

        //draw
        drawEntity(*light);
    }
}

void Renderer::drawSceneSkybox(const Scene &scene, const SkyboxShaderProgram &shaderProgram, const Camera &camera) const
{
    // TODO

    for (const Skybox *skybox : scene.getSkybox())
    {

        shaderProgram.applyEntityTransformation(*skybox);
        shaderProgram.setSkyboxMaterial(*skybox);
        shaderProgram.setViewPosition(camera.getPosition()); // TODO: set camera positon
        shaderProgram.setSkyboxColor(*skybox);

        //const BaseObjectModel &model = skybox->getModel();
        // glDepthMask(GL_FALSE);
        // glBindVertexArray(model.getVAO()); //bind model VAO
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_CUBE_MAP, skybox->getTextureID());
        // glDrawArrays(GL_TRIANGLES, 0, model.getVerticesSize()); // draw
        // glBindVertexArray(0);
        // glDepthMask(GL_TRUE);
        drawEntity(*skybox);
    }
}

void Renderer::render(const Scene &scene, const AbsorberShaderProgram &absorberShaderProgram,
                      const LightShaderProgram &lightShaderProgram, const SkyboxShaderProgram &skyboxShaderProgram, const Camera &camera) const
{
    drawBackground();

    skyboxShaderProgram.use();
    skyboxShaderProgram.applyView(camera);
    skyboxShaderProgram.applyProjection(camera);
    drawSceneSkybox(scene, skyboxShaderProgram, camera);

    absorberShaderProgram.use();
    absorberShaderProgram.applyView(camera);
    absorberShaderProgram.applyProjection(camera);
    drawSceneAbsorbers(scene, camera, absorberShaderProgram);

    absorberShaderProgram.use();
    absorberShaderProgram.applyView(camera);
    absorberShaderProgram.applyProjection(camera);
    drawSceneAbsorbers(scene, camera, absorberShaderProgram);

    lightShaderProgram.use();
    drawSceneLights(scene, lightShaderProgram);
    lightShaderProgram.applyView(camera);
    lightShaderProgram.applyProjection(camera);
}