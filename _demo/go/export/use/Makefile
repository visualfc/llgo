# Makefile for C demo using Go exported library
# Use LINK_TYPE environment variable to choose library type:
# LINK_TYPE=static  - Link with static library (default)
# LINK_TYPE=shared  - Link with shared library

CC = clang
CFLAGS = -Wall -Wextra -std=c99
INCLUDES = -I..
TARGET = main.out
SOURCES = main.c
HEADER = ../libexport.h
RUNTIME_LIBS = -lpthread -lm $(shell pkg-config --libs bdw-gc || echo -lgc) $(shell pkg-config --libs libuv || echo -luv)

# Default to static linking
LINK_TYPE ?= static
LLGOFLAGS ?=

# Platform detection
UNAME_S := $(shell uname -s)
WINDOWS := $(filter MINGW% MSYS% CYGWIN%,$(UNAME_S))
SHARED_LINK_FLAGS = -L.. -lexport
ifeq ($(UNAME_S),Darwin)
    SHARED_EXT = dylib
    PLATFORM_LIBS =
else ifneq ($(WINDOWS),)
    SHARED_EXT = dll
    GOARCH = $(shell go env GOARCH)
    MSVC_TARGET_386 = i686-pc-windows-msvc
    MSVC_TARGET_amd64 = x86_64-pc-windows-msvc
    MSVC_TARGET_arm64 = aarch64-pc-windows-msvc
    CFLAGS += --target=$(MSVC_TARGET_$(GOARCH)) -Wl,/debug:dwarf
    SHARED_LINK_FLAGS = ../libexport.lib
    PLATFORM_LIBS = -lsynchronization -lws2_32
    MSYS2_LIB_DIR = $(shell pkg-config --variable=libdir bdw-gc)
    LIBUV_SYSTEM_LIBS = $(filter-out -L% -luv,$(shell pkg-config --libs libuv))
    # Keep the C consumer on LLGo's MSVC ABI. lld-link accepts these controlled
    # MSYS2 import archives by path; -l would search for incompatible names.
    RUNTIME_LIBS = "$(MSYS2_LIB_DIR)/libgc.dll.a" "$(MSYS2_LIB_DIR)/libuv.dll.a" $(LIBUV_SYSTEM_LIBS)
    STATIC_STDLIB_LIBS = "$(MSYS2_LIB_DIR)/libffi.dll.a"
else
    SHARED_EXT = so
    PLATFORM_LIBS = $(shell pkg-config --libs libunwind 2>/dev/null || echo -lunwind)
endif
ifeq ($(WINDOWS),)
    STATIC_STDLIB_LIBS = $(shell pkg-config --libs libffi 2>/dev/null || echo -lffi) -lresolv
endif

# Library and flags based on link type
ifeq ($(LINK_TYPE),shared)
    BUILDMODE = c-shared
    OUTPUT = libexport.$(SHARED_EXT)
    LIBRARY = ../$(OUTPUT)
    LDFLAGS = $(SHARED_LINK_FLAGS) $(RUNTIME_LIBS) $(PLATFORM_LIBS)
    BUILD_MSG = "Building Go shared library..."
    LINK_MSG = "Linking with shared library..."
else
    BUILDMODE = c-archive
    OUTPUT = libexport.a
    LIBRARY = ../$(OUTPUT)
    LDFLAGS = $(LIBRARY) $(RUNTIME_LIBS) $(PLATFORM_LIBS) $(STATIC_STDLIB_LIBS)
    BUILD_MSG = "Building Go static library..."
    LINK_MSG = "Linking with static library..."
endif

.PHONY: all clean run build-go

all: build-go $(TARGET)

# Build the Go library first
build-go:
	@echo $(BUILD_MSG)
	cd .. && ../../../dev/llgo.sh build -buildmode $(BUILDMODE) $(LLGOFLAGS) -o $(OUTPUT) .

# Build the C executable
$(TARGET): $(SOURCES) $(LIBRARY) $(HEADER)
	@echo $(LINK_MSG)
	$(CC) $(CFLAGS) $(INCLUDES) -o $(TARGET) $(SOURCES) $(LDFLAGS)

# Run the executable
run: $(TARGET)
	@echo "Running C demo..."
ifeq ($(LINK_TYPE),shared)
	@echo "Setting library path for shared library..."
ifneq ($(WINDOWS),)
	PATH=..:$$PATH ./$(TARGET)
else
	LD_LIBRARY_PATH=.. DYLD_LIBRARY_PATH=.. ./$(TARGET)
endif
else
	./$(TARGET)
endif

# Clean build artifacts
clean:
	rm -f $(TARGET)
	rm -f ../libexport.a ../libexport.h ../libexport.so ../libexport.dylib ../libexport.dll ../libexport.lib

# Help target
help:
	@echo "Available targets:"
	@echo "  all       - Build Go library and C executable"
	@echo "  build-go  - Build only the Go library"
	@echo "  run       - Build and run the C demo"
	@echo "  clean     - Clean all build artifacts"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Environment variables:"
	@echo "  LINK_TYPE - Library type: 'static' (default) or 'shared'"
	@echo ""
	@echo "Examples:"
	@echo "  make run                    # Use static library"
	@echo "  LINK_TYPE=shared make run   # Use shared library"
