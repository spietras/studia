#version 330 core

uniform vec4 lightColor;

out vec4 FragColor;

void main()
{
    FragColor = lightColor;
}