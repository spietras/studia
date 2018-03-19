#COMPILER
CC = g++
CFLAGS = -Wall -std=c++11

#VERSION
ifeq ($(BUILD),release)
	CFLAGS += -O3 -s -DNDEBUG
else
	CFLAGS += -O0 -g
	BUILD = debug
endif

#FOLDERS
OBJ = $(BUILD)/obj

#TARGET
TARGET = $(BUILD)/Projekt1.out

#FILES
SOURCE_FILES= main.cpp node.cpp binarytree.cpp
OBJECT_FILES = $(SOURCE_FILES:%.cpp=$(OBJ)/%.o)

all: $(TARGET)

debug:
	@make --no-print-directory

release:
	@make "BUILD=release" --no-print-directory

clean:
	rm -r -f debug
	rm -r -f release

.PHONY: all debug release clean

$(TARGET): $(OBJECT_FILES)
	$(info Building $@, version: $(BUILD))
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -o $@ $^
	$(info Build successful)

$(OBJECT_FILES): $(OBJ)/%.o: %.cpp
	$(info Building $@)
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -c -o $@ $<