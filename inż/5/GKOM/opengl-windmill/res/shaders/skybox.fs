#version 330 core
in vec3 normal;

uniform vec4 objectColor;
out vec4 FragColor;

in vec3 TexCoords;
uniform samplerCube skybox;

void main()
{
    // FragColor = objectColor;
    FragColor = texture(skybox, TexCoords);

}