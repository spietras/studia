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
    glBindVertexArray(model.getVAO()); //bind model VAO
    glDrawArrays(GL_TRIANGLES, 0, model.getVerticesSize()); // draw
    glBindVertexArray(0);
}

void Renderer::drawSceneAbsorbers(const Scene &scene, const Camera &camera, const AbsorberShaderProgram &shaderProgram) const
{
    const std::vector<const PointLight *> &lights = scene.getLights();
    shaderProgram.setLightsNumber(lights.size());

    for (const Absorber *absorber : scene.getAbsorbers())
    {
        shaderProgram.applyEntityTransformation(*absorber);
        shaderProgram.setAbsorberMaterial(*absorber);
        shaderProgram.setViewPosition(camera.getPosition());

        for (int i = 0; i < lights.size(); i++)
            shaderProgram.setLight(*lights[i], i);

        drawEntity(*absorber);
    }
}

void Renderer::drawSceneLights(const Scene &scene, const LightShaderProgram &shaderProgram) const
{
    for (const PointLight *light : scene.getLights())
    {
        shaderProgram.applyEntityTransformation(*light);
        shaderProgram.setLightColor(*light);
        drawEntity(*light);
    }
}

void Renderer::render(const Scene &scene, const Camera &camera, const AbsorberShaderProgram &absorberShaderProgram,
                      const LightShaderProgram &lightShaderProgram) const
{
    drawBackground();
    absorberShaderProgram.use();
    drawSceneAbsorbers(scene, camera, absorberShaderProgram);
    absorberShaderProgram.applyView(camera);
    absorberShaderProgram.applyProjection(camera);
    lightShaderProgram.use();
    drawSceneLights(scene, lightShaderProgram);
    lightShaderProgram.applyView(camera);
    lightShaderProgram.applyProjection(camera);
}