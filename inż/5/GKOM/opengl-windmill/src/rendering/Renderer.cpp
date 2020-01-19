#include "Renderer.h"

void Renderer::drawBackground() const
{
    glClearColor(backgroundColor.red,
                 backgroundColor.green,
                 backgroundColor.blue,
                 backgroundColor.alpha);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void Renderer::drawSceneAbsorbersDepth(const Scene &scene, const DepthShaderProgram &shaderProgram) const
{
    const DirectionalLight *light = scene.getDirectionalLight();
    shaderProgram.applyLightSpace(*light); // TODO: handle no directional light

    for (const Absorber *absorber : scene.getAbsorbers())
    {
        shaderProgram.applyEntityTransformation(*absorber);

        drawEntity(*absorber);
    }
}

void Renderer::drawEntity(const Entity &entity) const
{
    const BaseObjectModel &model = entity.getModel();
    glBindVertexArray(model.getVAO()); //bind model VAO
    glDrawArrays(GL_TRIANGLES, 0, model.getVerticesSize()); //draw
    glBindVertexArray(0);
}

void
Renderer::drawSceneAbsorbers(const Scene &scene, const Camera &camera, const AbsorberShaderProgram &shaderProgram) const
{
    const std::vector<const PointLight *> &lights = scene.getLights();
    shaderProgram.setLightsNumber(lights.size());

    const DirectionalLight *directionalLight = scene.getDirectionalLight(); // TODO: handle no directional light
    if (directionalLight != nullptr)
    {
        shaderProgram.setDirectionlight(*directionalLight);
        shaderProgram.setShadowMap(directionalLight->getDepthTextureUnit());
        glActiveTexture(directionalLight->getDepthTextureUnit());
        glBindTexture(GL_TEXTURE_2D, directionalLight->getDepthMap());
    }

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

void Renderer::renderShadowMap(const Scene &scene, const DepthShaderProgram &depthShaderProgram) const
{
    //draw to depth framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, scene.getDirectionalLight()->getDepthFBO()); // TODO: handle no directional light
    glClear(GL_DEPTH_BUFFER_BIT);

    depthShaderProgram.use();
    drawSceneAbsorbersDepth(scene, depthShaderProgram);

    //return to default framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void Renderer::render(const Scene &scene,
                      const Camera &camera,
                      const AbsorberShaderProgram &absorberShaderProgram,
                      const LightShaderProgram &lightShaderProgram) const
{
    drawBackground();

    absorberShaderProgram.use();
    absorberShaderProgram.applyView(camera);
    absorberShaderProgram.applyProjection(camera);
    drawSceneAbsorbers(scene, camera, absorberShaderProgram);

    lightShaderProgram.use();
    drawSceneLights(scene, lightShaderProgram);
    lightShaderProgram.applyView(camera);
    lightShaderProgram.applyProjection(camera);
}