#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNorm;

uniform mat4 model;

out vec3 normal;

void main()
{
    gl_Position = model * vec4(aPos, 1.0);
    normal = aNorm;
}
