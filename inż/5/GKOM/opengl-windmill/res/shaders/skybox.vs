#version 330 core

layout (location = 0) in vec3 aPos; // vertex position from VBO
layout (location = 1) in vec3 aNorm; // vertex normal from VBO
layout (location = 2) in vec3 aTex;


uniform mat4 model; // model matrix
uniform mat4 view;  //view matrix
uniform mat4 projection;  //projection matrix

out vec3 fragPos; // fragment position in world space
out vec3 normal; // normal in world space
out vec3 TexCoords;

void main()
{
    TexCoords = aPos;
    // multiplying by model matrix to get into world space
    fragPos = vec3(model * vec4(aPos, 1.0));
    normal = vec3(model * vec4(-aNorm, 1.0));

    // screen space position
    gl_Position = projection * view * vec4(fragPos, 1.0);
}
