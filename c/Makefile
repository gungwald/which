# Project: which

CPP = g++
CC  = gcc
WINDRES = windres
RES = 
OBJ = which.o $(RES)
LINKOBJ = which.o $(RES)
BIN = which.exe
CXXFLAGS = $(CXXINCS)  
CFLAGS = $(INCS)  

$(BIN): $(OBJ) 
	$(CC) $(LINKOBJ) -o $(BIN) $(LIBS)

which.o: which.c

clean: clean-custom
	del $(OBJ) $(BIN)

