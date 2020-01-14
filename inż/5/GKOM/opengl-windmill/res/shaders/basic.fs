#version 330 core

in vec3 normal;

uniform vec4 objectColor;

out vec4 FragColor;

void main()
{
    FragColor = objectColor;
}
