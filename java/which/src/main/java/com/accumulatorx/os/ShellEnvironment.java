package com.accumulatorx.os;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.StringTokenizer;


public class ShellEnvironment extends HashMap<String,String> implements Environment {

	private static final long serialVersionUID = 1L;

	public ShellEnvironment() throws IOException {
    	
        ArrayList<String> output = captureCommandOutput(buildPrintEnvCommand());
        for (String line : output) {
            String[] parts = line.split("=", 2);
            put(parts[0], parts[1]);
        }
    }
    
    public ArrayList<String> captureCommandOutput(String command) throws IOException {
        ArrayList<String> output = new ArrayList<String>();
        Runtime rt = Runtime.getRuntime();
        Process proc = rt.exec(command);
        BufferedReader reader = new BufferedReader(new InputStreamReader(proc.getInputStream()));
        try {
            output.add(reader.readLine());
            proc.waitFor();
        }
        catch (InterruptedException ie) {
            ie.printStackTrace();
        }
        finally {
            close(reader);
        }
        return output;
    }
    
    public void close(Reader reader) {
        if (reader != null) {
            try {
                reader.close();
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public String buildGetEnvCommand(String varName) {
        String getenvCommand = "/bin/sh -c echo $" + varName;
        if (isWindows()) {
            // Must first check if the variable is defined on Windows
            // because it will print the variable name if it is not
            // defined. We don't want that.
            getenvCommand = "cmd /c if defined " + varName + " echo %" + varName + "%";
        }
        return getenvCommand;
    }
    
    public String buildPrintEnvCommand() {
        String printenvCommand = "/usr/bin/printenv";
        if (isWindows()) {
            printenvCommand = "cmd /c set";
        }
        return printenvCommand;
    }
    
    public boolean isWindows() {
        return System.getProperty("os.name").startsWith("Windows");
    }
    
    public String get(String name) throws IOException {
        String value = null;
        BigDecimal javaSpecVersion = new BigDecimal(System.getProperty("java.specification.version"));
        
        if (javaSpecVersion.compareTo(new BigDecimal(1.0)) > 0 && javaSpecVersion.compareTo(new BigDecimal(1.5)) < 0) {
            // These versions don't have a built-in getenv function so 
            // use this custom JNI version.
            try {
                // With a native executable compiled with gcj this
                // method will already be compiled in. Otherwise
                // it the dll needs to be loaded.
                value = PlainPosix.getenv(name);
            }
            catch (UnsatisfiedLinkError ule) {
                try {
                    System.loadLibrary("getenv");
                    value = PlainPosix.getenv(name);
                }
                catch (Exception exc) {
                    // If we can't load the JNI library, call the shell.
                    ArrayList<String> output = captureCommandOutput(buildGetEnvCommand(name));
                    if (output.size() > 0) {
                        value = output.get(0);
                    }
                }
            }
        }
        else {
            value = System.getenv(name);
        }
        return value;
    }
    
    public List<String> getMultiValue(String name) throws IOException {
        return split(System.getProperty("path.separator"), get(name));
    }

    public List<String> split(String splittor, String s) {
        if (s == null) {
            return null;
        }
        ArrayList<String> parts = new ArrayList<String>();
        StringTokenizer tokenizer = new StringTokenizer(s, splittor);
        while (tokenizer.hasMoreTokens()) {
            parts.add(tokenizer.nextToken());
        }
        return parts;
    }

}
