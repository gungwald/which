# Project: Which
# Makefile created by Dev-C++ 4.9.8.0

CPP  = g++
CC   = gcc
WINDRES = windres.exe
RES  = 
OBJ  = main.o $(RES)
LINKOBJ  = main.o $(RES)
LIBS =
INCS =
CXXINCS =
BIN  = which
CXXFLAGS = $(CXXINCS)-g  
CFLAGS = $(INCS) 

.PHONY: all all-before all-after clean clean-custom

all: all-before which all-after


clean: clean-custom
	rm -f $(OBJ) $(BIN)

$(BIN): $(LINKOBJ)
	$(CPP) $(LINKOBJ) -o which $(LIBS)

main.o: main.cpp
	$(CPP) -c main.cpp -o main.o $(CXXFLAGS)
