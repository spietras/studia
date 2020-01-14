#include "Renderer.h"

void Renderer::drawBackground() const
{
    glClearColor(backgroundColor.red,
                 backgroundColor.green,
                 backgroundColor.blue,
                 backgroundColor.alpha);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void Renderer::drawScene(const Scene &scene, const ShaderProgram &shaderProgram) const
{
    for (const Entity * entity : scene.getEntities())
    {
        shaderProgram.applyEntityTransformation(*entity);
        shaderProgram.setEntityColor(*entity);
        const BaseObjectModel &model = entity->getModel();
        glBindVertexArray(model.getVAO()); //bind model VAO
        glDrawArrays(GL_TRIANGLES, 0, model.getVerticesSize()); // draw
        glBindVertexArray(0);
    }
}

void Renderer::render(const Scene &scene, const ShaderProgram &shaderProgram) const
{
    drawBackground();
    shaderProgram.use();
    drawScene(scene, shaderProgram);
}