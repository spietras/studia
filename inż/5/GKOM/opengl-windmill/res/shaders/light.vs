#version 330 core
layout (location = 0) in vec3 aPos; // vertex position from VBO

uniform mat4 model; // model matrix

void main()
{
    // screen space position
	gl_Position = model * vec4(aPos, 1.0);
}