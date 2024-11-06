# Makefile for gpp_lexer

# Compiler and flags
CC = gcc
FLEX = flex
FLEX_FLAGS = -o
CFLAGS = -ll

# Source and target files
LEXER_SRC = gpp_lexer.l
LEXER_C = bin/gpp_lexer.c
TARGET = bin/gpp_lexer

# Default target
all: $(TARGET)

# Rule to build the lexer
$(TARGET): $(LEXER_C)
	$(CC) -o $(TARGET) $(LEXER_C) $(CFLAGS)

# Rule to generate C code from Flex file
$(LEXER_C): $(LEXER_SRC)
	@mkdir -p bin
	$(FLEX) $(FLEX_FLAGS) $(LEXER_C) $(LEXER_SRC)

# Clean up generated files
clean:
	rm -rf bin

.PHONY: all clean