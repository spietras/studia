cmake_minimum_required(VERSION 3.0)

set(TARGET "Wiatrak")

set (CMAKE_CXX_FLAGS "-O0 -no-pie")

# change these to proper directories where each file should be
set(SRCDIR "src")
set(BINDIR "bin")
set(LIBDIR "lib/Linux")
set(INCLUDEDIR "include")

project(${TARGET})

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/${BINDIR})

include_directories(${CMAKE_SOURCE_DIR}/${INCLUDEDIR})

add_executable( ${TARGET}
                ${SRCDIR}/main.cpp
                ${SRCDIR}/rendering/Renderer.cpp
                ${SRCDIR}/rendering/Window.cpp
                ${SRCDIR}/rendering/Window.h
                ${SRCDIR}/utils/Color.h
                ${SRCDIR}/entities/models/BaseObjectModel.cpp
                ${SRCDIR}/entities/models/BaseObjectModel.h
                ${SRCDIR}/rendering/shaders/ShaderProgram.cpp
                ${SRCDIR}/rendering/shaders/ShaderProgram.h
                ${SRCDIR}/entities/models/CuboidModel.h
                ${SRCDIR}/entities/models/CubeModel.h
                ${SRCDIR}/entities/Entity.cpp
                ${SRCDIR}/entities/Entity.h
                ${SRCDIR}/rendering/Scene.cpp
                ${SRCDIR}/rendering/Scene.h
                ${SRCDIR}/utils/Vertex.h
                ${SRCDIR}/rendering/shaders/LightShaderProgram.cpp
                ${SRCDIR}/rendering/shaders/LightShaderProgram.h
                ${SRCDIR}/rendering/shaders/AbsorberShaderProgram.cpp
                ${SRCDIR}/rendering/shaders/AbsorberShaderProgram.h
                ${SRCDIR}/entities/absorbers/Absorber.h
                ${SRCDIR}/entities/lights/PointLight.h
                ${SRCDIR}/utils/Material.h
                ${SRCDIR}/utils/PointLightAttributes.h
                ${SRCDIR}/rendering/shaders/LightShaderProgram.cpp
                ${SRCDIR}/rendering/Camera.cpp
                ${SRCDIR}/rendering/Camera.h
                ${SRCDIR}/entities/lights/DirectionalLight.h
                ${SRCDIR}/utils/DirectionalLightAttributes.h
                ${SRCDIR}/rendering/shaders/DepthShaderProgram.cpp
                ${SRCDIR}/rendering/shaders/DepthShaderProgram.h
                ${SRCDIR}/entities/lights/DirectionalLight.cpp
                ${SRCDIR}/entities/skybox/Skybox.h
                ${SRCDIR}/utils/Texture.h
                ${SRCDIR}/rendering/shaders/SkyboxShaderProgram.cpp
                ${SRCDIR}/rendering/shaders/SkyboxShaderProgram.h
                ${SRCDIR}/entities/models/PlaneModel.h
                ${SRCDIR}/entities/models/RectangularFrustum.h
                ${SRCDIR}/utils/GeometryHelper.h
                ${SRCDIR}/entities/models/Pyramid.h
                ${SRCDIR}/entities/models/RegularPyramid.h
                ${SRCDIR}/entities/models/CircularFrustum.h
                ${SRCDIR}/entities/models/Cylinder.h
                ${SRCDIR}/entities/models/Cone.h
                ${SRCDIR}/entities/models/Tetrahedron.h
                ${SRCDIR}/entities/models/RegularTetrahedron.h)

target_link_libraries(  ${TARGET}
                        ${CMAKE_SOURCE_DIR}/${LIBDIR}/libSOIL.a
                        ${CMAKE_SOURCE_DIR}/${LIBDIR}/libglfw3.a
                        -lX11
                        -lXrandr
                        -lXinerama
                        -lXi
                        -lXxf86vm
                        -lXcursor
                        -lpthread
                        ${CMAKE_SOURCE_DIR}/${LIBDIR}/libGLEW.a
                        -lGL)
