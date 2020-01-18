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

    const DirectionalLight* directionalLight = scene.getDirectionalLight();
    if (directionalLight != nullptr)
        shaderProgram.setDirectionlight(*directionalLight);

    for (const Absorber *absorber : scene.getAbsorbers())
    {
        //set shader uniforms
        shaderProgram.applyEntityTransformation(*absorber);
        shaderProgram.setAbsorberMaterial(*absorber);
        shaderProgram.setViewPosition(camera.getPosition());

        for (int i = 0; i < lights.size(); i++)
            shaderProgram.setLight(*lights[i], i);

        //draw
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
        shaderProgram.setViewPosition({0.0f, 0.0f, -1.0f}); // TODO: set camera positon
        shaderProgram.setSkyboxColor(*skybox);

        const BaseObjectModel &model = skybox->getModel();
        glBindVertexArray(model.getVAO()); //bind model VAO
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_CUBE_MAP, skybox->getTextureID());
        glDrawArrays(GL_TRIANGLES, 0, model.getVerticesSize()); // draw
        glBindVertexArray(0);
        // drawEntity(*skybox);
    }
}

void Renderer::render(const Scene &scene, const AbsorberShaderProgram &absorberShaderProgram,
                      const LightShaderProgram &lightShaderProgram, const SkyboxShaderProgram &skyboxShaderProgram, const Camera &camera) const
{
    drawBackground();
    skyboxShaderProgram.use();
    drawSceneSkybox(scene, skyboxShaderProgram);
    absorberShaderProgram.use();
    absorberShaderProgram.applyView(camera);
    absorberShaderProgram.applyProjection(camera);
    drawSceneAbsorbers(scene, camera, absorberShaderProgram);

    lightShaderProgram.use();
    drawSceneLights(scene, lightShaderProgram);
    lightShaderProgram.applyView(camera);
    lightShaderProgram.applyProjection(camera);
}