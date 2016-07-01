#
# Builds which.jar with standard Java or OpenJDK and MinGW gcc.
# Make sure the bit length of Java and gcc match 64 with 64 or 32 with 32.
#
# Warning: Windows will not grab the getenv.dll in the local directory
# by default if there is another one in the PATH. Argh. I though I was
# generating a 32-bit getenv.dll for a long time because it was still
# grabing the old 32-bit version at runtime. Use "depends" to check the
# bitness of the getenv.dll file.
#
# Requrements:
# JAVA_HOME must be set correctly.
#

CC=gcc
INSTALL_PREFIX=$(USERPROFILE)\Documents

all: which.jar getenv.dll


which.jar: Which.class PlainPosix.class
	jar -cvfe which.jar Which Which.class PlainPosix.class


getenv.dll: getenv.o
	$(CC) -shared -Wl,--kill-at -o getenv.dll getenv.o


getenv.o: getenv.c PlainPosix.h
	$(CC) -c -Wall -D_JNI_IMPLEMENTATION_ -I$(JAVA_HOME)\include -I$(JAVA_HOME)\include\win32 -o getenv.o getenv.c
	

Which.class: Which.java PlainPosix.class
	javac -classpath . Which.java


PlainPosix.h: PlainPosix.class
	javah -classpath . -jni PlainPosix


PlainPosix.class: PlainPosix.java
	javac PlainPosix.java


install: all
	copy which.jar $(INSTALL_PREFIX)\lib
	copy getenv.dll $(INSTALL_PREFIX)\bin
	copy which.bat $(INSTALL_PREFIX)\bin


clean:
	del *.o *.class PlainPosix.h which.jar getenv.dll

