# all source files (like .c or .h) should be in SRCDIR
# all object files will be in OBJDIR
# executable will be in BINDIR
# static libraries (lib<name>.a files should be in LIBDIR)

# project name (generate executable with this name)
TARGET   = Wiatrak

# change these to proper directories where each file should be
SRCDIR     = src
OBJDIR     = obj
BINDIR     = bin
LIBDIR	   = lib/Linux
INCLUDEDIR = include

CC       = g++
# c compiling flags here
CFLAGS   = -Wall -g -I$(INCLUDEDIR)

LINKER   = g++
# linking flags here
# needed libraries:
# SOIL - static
# GLEW - static
# glfw3 - static
# glfw3 dependecies: X11, Xrandr, Xinerama, Xi, Xxf86vm, Xcursor, pthread - dynamic
# GL - dynamic
LFLAGS   = -Wall -no-pie -L$(LIBDIR) -Wl,-Bstatic -lSOIL -lGLEW -lglfw3 -Wl,-Bdynamic -lX11 -lXrandr -lXinerama -lXi -lXxf86vm -lXcursor -lpthread -lGL -Wl,--as-needed

# resursive wildcard function, thanks to: https://stackoverflow.com/a/12959764
rwildcard=$(wildcard $1/$2) $(foreach d,$(wildcard $1/*),$(call rwildcard,$d,$2))

CSOURCES  := $(call rwildcard,$(SRCDIR),*.cpp)
COBJECTS  := $(CSOURCES:$(SRCDIR)/%.cpp=$(OBJDIR)/%.o)
rm       = rm -rf

# link all objects and build executable
$(BINDIR)/$(TARGET): $(COBJECTS)
	@mkdir -p $(@D)
	@$(LINKER) $(COBJECTS) $(LFLAGS) -o $@
	@echo "Linking complete!"

# compile each source into object
$(COBJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.cpp
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully!"

.PHONY: clean
clean:
	@$(rm) $(OBJDIR)
	@$(rm) $(BINDIR)
	@echo "Cleanup complete!"
