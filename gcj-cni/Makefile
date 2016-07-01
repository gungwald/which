which.exe: getenv.o Posix.o Which.o
	gcj -o which.exe --main=Which getenv.o Posix.o Which.o

Which.o: Which.java
	gcj -c -classpath . Which.java

getenv.o: Posix.h getenv.cpp
	g++ -c getenv.cpp

Posix.o: Posix.class
	gcj -c Posix.class

Posix.h: Posix.class
	gcjh -classpath . Posix

Posix.class: Posix.java
	gcj -C Posix.java

clean:
	del -rf which.exe *.class *.o Posix.h
