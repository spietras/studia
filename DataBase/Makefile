# project name (generate executable with this name)
TARGET   = DataBase

CC       = gcc
# c compiling flags here
CFLAGS   = -Wall -g

LINKER   = gcc
# linking flags here
LFLAGS   = -Wall

# change these to proper directories where each file should be
SRCDIR   = src
OBJDIR   = obj
BINDIR   = bin

CSOURCES  := $(wildcard $(SRCDIR)/*.c)
COBJECTS  := $(CSOURCES:$(SRCDIR)/%.c=$(OBJDIR)/%.o)
rm       = rm -rf

# link all objects and build executable
$(BINDIR)/$(TARGET): $(COBJECTS)
	@mkdir -p $(@D)
	@$(LINKER) $(COBJECTS) $(LFLAGS) -o $@
	@echo "Linking complete!"

# compile each source into object
$(COBJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.c
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully!"

.PHONY: clean
clean:
	@$(rm) $(OBJDIR)
	@$(rm) $(BINDIR)
	@echo "Cleanup complete!"