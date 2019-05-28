# project name (generate executable with this name)
TARGET   = Huffman

CC       = g++
# c compiling flags here
CFLAGS   = 

ASM		 = nasm
# asm compiling flags here
ASMFLAGS = -felf64

LINKER   = g++
# linking flags here
LFLAGS   = 

# change these to proper directories where each file should be
SRCDIR   = src
OBJDIR   = obj
BINDIR   = bin

CPPSOURCES  := $(wildcard $(SRCDIR)/*.cpp)
ASMSOURCES  := $(wildcard $(SRCDIR)/*.asm)
CPPOBJECTS  := $(CPPSOURCES:$(SRCDIR)/%.cpp=$(OBJDIR)/%.o)
ASMBOJECTS	:= $(ASMSOURCES:$(SRCDIR)/%.asm=$(OBJDIR)/%.o)
rm       = rm -rf

$(BINDIR)/$(TARGET): $(CPPOBJECTS) $(ASMBOJECTS)
	@mkdir -p $(@D)
	@$(LINKER) $(CPPOBJECTS) $(ASMBOJECTS) $(LFLAGS) -o $@
	@echo "Linking complete!"

$(CPPOBJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.cpp
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully!"

$(ASMBOJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.asm
	@mkdir -p $(@D)
	@$(ASM) $(ASMFLAGS) $< -o $@
	@echo "Compiled "$<" successfully!"

.PHONY: clean
clean:
	@$(rm) $(OBJDIR)
	@$(rm) $(BINDIR)
	@echo "Cleanup complete!"