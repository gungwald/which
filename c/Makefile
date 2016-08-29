# Project: which

BIN = which.exe
CC = gcc
WINDRES = windres
RES = 
LINKOBJ = which.o errors.o stringconcat.o $(RES)
CFLAGS = -std=c11 -c -g -O3 -DUNICODE -D_UNICODE $(INCS)  

$(BIN): $(LINKOBJ)
	$(CC) $(LINKOBJ) -o $(BIN) $(LIBS)

which.o: which.c errors.h stringconcat.h
	$(CC) $(CFLAGS) $<

errors.o: errors.c errors.h
	$(CC) $(CFLAGS) $<

stringconcat.o: stringconcat.c stringconcat.h
	$(CC) $(CFLAGS) $<

clean:
	del $(LINKOBJ) *.gch $(BIN)

