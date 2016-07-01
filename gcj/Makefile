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
# JAVA_HOME must be set correctly so that the JNI include files can be found
#

CC=gcc

JAVAC=gcj
JAVAH=gcjh
JAR=gjar

INSTALL_PREFIX=$(USERPROFILE)\Documents

all: getenv.dll which.exe


# No jar command unfortuantely
#which.jar: Which.class PlainPosix.class
#$(JAR) -cvfe which.jar Which Which.class PlainPosix.class


getenv.dll: getenv.o
	$(CC) -shared -Wl,--kill-at -o getenv.dll getenv.o


getenv.o: getenv.c PlainPosix.h
	$(CC) -c -Wall -D_JNI_IMPLEMENTATION_ -I"$(JAVA_HOME)"\include -I"$(JAVA_HOME)"\include\win32 -o getenv.o getenv.c
	

Which.class: PlainPosix.class Which.java
	$(JAVAC) -classpath . -C Which.java


PlainPosix.h: PlainPosix.class
	$(JAVAH) -classpath . -jni PlainPosix


PlainPosix.class: PlainPosix.java
	$(JAVAC) -C PlainPosix.java


install: all
	copy which.jar $(INSTALL_PREFIX)\lib
	copy getenv.dll $(INSTALL_PREFIX)\bin
	copy which.bat $(INSTALL_PREFIX)\bin
	if exist which.exe copy which.exe $(INSTALL_PREFIX)\bin


clean:
	del *.o *.class PlainPosix.h which.jar getenv.dll which.exe


##############
# GCJ Specific
#
BUNDLE=MessagesBundle.properties.o

which.exe: Which.o getenv.o PlainPosix.o $(BUNDLE)
	gcj -o which.exe --main=Which -D_JNI_IMPLEMENTATION_ \
		$(BUNDLE) getenv.o PlainPosix.o Which.o

Which.o: Which.class
	gcj -classpath . -c -fjni Which.class

PlainPosix.o: PlainPosix.class
	gcj -classpath . -c -fjni PlainPosix.class

$(BUNDLE):
	ar x $(JAVA_HOME)\lib\libgcj.a $(BUNDLE) 

