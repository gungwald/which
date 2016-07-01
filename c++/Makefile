# Project: Which
# Makefile created by Dev-C++ 4.9.8.0

CPP  = g++.exe
CC   = gcc.exe
WINDRES = windres.exe
RES  = 
OBJ  = main.o $(RES)
LINKOBJ  = main.o $(RES)
LIBS =  -L"C:/opt/Dev-Cpp/lib" 
INCS =  -I"C:/opt/Dev-Cpp/include" 
CXXINCS =  -I"C:/opt/Dev-Cpp/include/c++"  -I"C:/opt/Dev-Cpp/include/c++/mingw32"  -I"C:/opt/Dev-Cpp/include/c++/backward"  -I"C:/opt/Dev-Cpp/include" 
BIN  = Which.exe
CXXFLAGS = $(CXXINCS)-g  
CFLAGS = $(INCS) 

.PHONY: all all-before all-after clean clean-custom

all: all-before Which.exe all-after


clean: clean-custom
	rm -f $(OBJ) $(BIN)

$(BIN): $(LINKOBJ)
	$(CPP) $(LINKOBJ) -o "Which.exe" $(LIBS)

main.o: main.cpp
	$(CPP) -c main.cpp -o main.o $(CXXFLAGS)
