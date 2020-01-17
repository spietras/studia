#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNorm;

uniform mat4 model;

out vec3 fragPos;
out vec3 normal;

void main()
{
    fragPos = vec3(model * vec4(aPos, 1.0));
    normal = vec3(model * vec4(aNorm, 1.0));

    gl_Position = vec4(fragPos, 1.0);
}
