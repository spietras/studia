cmake_minimum_required(VERSION 3.0)

set(TARGET "Wiatrak")

# change these to proper directories where each file should be
set(SRCDIR "src")
set(BINDIR "bin")
set(LIBDIR "lib/Linux")
set(INCLUDEDIR "include")

project(${TARGET})

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${BINDIR})

include_directories(${CMAKE_SOURCE_DIR}/${INCLUDEDIR})

add_executable( ${TARGET}
                ${SRCDIR}/main.cpp
                ${SRCDIR}/Renderer.cpp
                ${SRCDIR}/utils/Window.cpp
                ${SRCDIR}/utils/Window.h
                ${SRCDIR}/utils/Color.h)

target_link_libraries(${TARGET}
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
