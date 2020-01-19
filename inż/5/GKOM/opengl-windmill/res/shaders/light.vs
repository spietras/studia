#version 330 core
layout (location = 0) in vec3 aPos; // vertex position from VBO

uniform mat4 model; // model matrix
uniform mat4 view;  //view matrix
uniform mat4 projection;  //projection matrix

void main()
{
    // screen space position
	gl_Position = projection * view * model * vec4(aPos, 1.0);
}