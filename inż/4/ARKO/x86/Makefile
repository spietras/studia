# project name (generate executable with this name)
TARGET   = Huffman

CC       = gcc
# c compiling flags here
CFLAGS   = -Wall

ASM		 = nasm
# asm compiling flags here
ASMFLAGS = -felf64 -s -Wall

LINKER   = gcc
# linking flags here
LFLAGS   = -Wall

# change these to proper directories where each file should be
SRCDIR   = src
OBJDIR   = obj
BINDIR   = bin

CSOURCES  := $(wildcard $(SRCDIR)/*.c)
ASMSOURCES  := $(wildcard $(SRCDIR)/*.asm)
COBJECTS  := $(CSOURCES:$(SRCDIR)/%.c=$(OBJDIR)/%.o)
ASMBOJECTS	:= $(ASMSOURCES:$(SRCDIR)/%.asm=$(OBJDIR)/%.o)
rm       = rm -rf

$(BINDIR)/$(TARGET): $(COBJECTS) $(ASMBOJECTS)
	@mkdir -p $(@D)
	@$(LINKER) $(COBJECTS) $(ASMBOJECTS) $(LFLAGS) -o $@
	@echo "Linking complete!"

$(COBJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.c
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