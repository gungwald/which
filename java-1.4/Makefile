Which.class: Which.java
	javac Which.java

install: Which.class
	install -Cv which /usr/local/bin
	install -dv /usr/local/share/java
	install -Cv -m 0644 Which.class /usr/local/share/java
