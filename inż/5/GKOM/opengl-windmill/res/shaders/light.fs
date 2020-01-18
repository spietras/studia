#version 330 core

uniform vec4 lightColor;

out vec4 FragColor;

void main()
{
    // just set fragment color to light color, don't do any shading
    FragColor = lightColor;
}