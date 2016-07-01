# Project: which

CPP = g++
CC  = gcc
WINDRES = windres
RES = 
OBJ = which.o $(RES)
LINKOBJ = which.o $(RES)
#LIBS = -L"C:/opt/Dev-Cpp/lib"  
#INCS = -I"C:/opt/Dev-Cpp/include" 
#CXXINCS = -I"C:/opt/Dev-Cpp/include/c++/3.3.1"  -I"C:/opt/Dev-Cpp/include/c++/3.3.1/mingw32"  -I"C:/opt/Dev-Cpp/include/c++/3.3.1/backward"  -I"C:/opt/Dev-Cpp/lib/gcc-lib/mingw32/3.3.1/include"  -I"C:/opt/Dev-Cpp/include" 
BIN = which.exe
CXXFLAGS = $(CXXINCS)  
CFLAGS = $(INCS)  

.PHONY: all all-before all-after clean clean-custom

all: all-before which.exe all-after


clean: clean-custom
	del $(OBJ) $(BIN)

$(BIN): $(OBJ)
	$(CC) $(LINKOBJ) -o which.exe $(LIBS)

