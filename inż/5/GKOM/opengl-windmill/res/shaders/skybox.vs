#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNorm;
layout (location = 2) in vec3 aTex;


uniform mat4 model;
out vec3 normal;
out vec3 TexCoords;

void main()
{
    TexCoords = aPos;
    gl_Position = model * vec4(aPos, 1.0);
    normal = aNorm;

}