#version 330 core

void main()
{
    // save depth info
    gl_FragDepth = gl_FragCoord.z;
}