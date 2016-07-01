/*
 * which - Find a command in the PATH in Windows
 * Copyright (c) 2002, 2015 Bill Chatfield
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

public class Which {
    
    public static final int EXIT_SUCCESS = 0;
    public static final int EXIT_FAILURE = 1;
    
    public HashMap env = new HashMap();
    
    private String[] path = null;
    private String[] pathExt = null;
    private boolean findAllRequested = false;
    private boolean verbose = false;
    private boolean substringSearchRequested = false;
    
    public static void main(String[] args) throws IOException {
        Which which = new Which();
        which.run(args);
    }
    
    public Which() throws IOException {
        // Build the env object, containing all environment variables and 
        // their values.
        try {
            ArrayList output = captureCommandOutput(buildPrintEnvCommand());
            for (int i = 0; i < output.size(); i++) {
                String line = output.get(i);
                String[] parts = line.split("=", 2);
                env.put(parts[0], parts[1]);
            }
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        
        pathExt = getMultiValuedEnvVar("PATHEXT");
        path = getMultiValuedEnvVar("PATH");
        if (path == null) {
            System.err.println("PATH environment variable is not set.");
            System.exit(EXIT_FAILURE);
        }

        if (isRunningOnWindows()) {
            // Add the assumed current directory to the path.
            path = push(path, System.getProperty("user.dir"));
        }
    }

    public boolean isRunningOnWindows() {
        return System.getProperty("os.name").startsWith("Windows");
    }
    
    public String[] split(String splittor, String s) {
        if (s == null) {
            return null;
        }
        ArrayList parts = new ArrayList();
        StringTokenizer tokenizer = new StringTokenizer(s, splittor);
        while (tokenizer.hasMoreTokens()) {
            parts.add(tokenizer.nextToken());
        }
        return parts.toArray(new String[parts.size()]);
    }

    public ArrayList captureCommandOutput(String command) throws IOException {
        ArrayList output = new ArrayList();
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
        if (isRunningOnWindows()) {
            // Must first check if the variable is defined on Windows
            // because it will print the variable name if it is not
            // defined. We don't want that.
            getenvCommand = "cmd /c if defined " + varName + " echo %" + varName + "%";
        }
        return getenvCommand;
    }
    
    public String buildPrintEnvCommand() {
        String printenvCommand = "/usr/bin/printenv";
        if (isRunningOnWindows()) {
            printenvCommand = "cmd /c set";
        }
        return printenvCommand;
    }
    
    public String getenv(String name) throws IOException {
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
                    ArrayList output = captureCommandOutput(buildGetEnvCommand(name));
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
    
    public String[] getMultiValuedEnvVar(String name) throws IOException {
        return split(System.getProperty("path.separator"), getenv(name));
    }
    
    /**
     * Implements the Perl/JavaScript push function.
     * 
     * @param receptor  What receiveth the pushed object, but only in the
     *                   returned object. The parameter itself is not modified.
     * @param pushed    That which is the object that is desired to be pushed
     * @return          An array one element bigger from the right
     */
    public static String[] push(String[] receptor, String pushed) {
        String[] bigger = new String[receptor.length + 1];
        for (int i = 0; i < receptor.length; i++) {
            bigger[i] = receptor[i];
        }
        bigger[receptor.length] = pushed; 
        return bigger;
    }

    public void run(String[] args) throws IOException {
        if (args.length < 1) {
            help();
            System.exit(EXIT_FAILURE);
        }

        for (String arg : args) {
            if (arg.equalsIgnoreCase("/v") || arg.equalsIgnoreCase("-v")) {
                setVerbose(true);
            }
            else if (arg.equalsIgnoreCase("/a") || arg.equalsIgnoreCase("-a")) {
                setFindAllRequested(true);
            }
            else if (arg.equalsIgnoreCase("/s") || arg.equalsIgnoreCase("-s")) {
                setSubstringSearchRequested(true);
            }
            else if (arg.equalsIgnoreCase("/?")
                     || arg.equalsIgnoreCase("/h")
                     || arg.equalsIgnoreCase("/help")
                     || arg.equalsIgnoreCase("--help")
                     || arg.equalsIgnoreCase("-help")
                     || arg.equalsIgnoreCase("-h")
                     || arg.equalsIgnoreCase("-?")) {
                help();
            }
            else if (arg.startsWith("/") || arg.startsWith("-")) {
                System.err.println("Invalid switch: " + arg);
                help();
                System.exit(EXIT_FAILURE);
            }
            else {
                for (File location : find(arg)) {
                    System.out.println(location.getCanonicalPath());
                }
            }
        }
    }
    
    public void setFindAllRequested(boolean value) {
        findAllRequested = value;
    }
    
    public boolean isFindAllRequested() {
        return findAllRequested;
    }
    
    public void setVerbose(boolean value) {
        verbose = value;
    }

    public boolean isVerbose() {
        return verbose;
    }

    public void setSubstringSearchRequested(boolean value) {
        this.substringSearchRequested = value;
    }

    public boolean isSubstringSearchRequested() {
        return substringSearchRequested;
    }

    public ArrayList find(String cmd) throws IOException {
        ArrayList matches = new ArrayList();
        String lowerCmd = cmd.toLowerCase();
        for (String dirName : path) {
            if (!findAllRequested && matches.size() > 0) {
                break;
            }
            File dir = new File(dirName);
            if (verbose) {
                System.out.println("   Looking in directory: " + dir.getAbsolutePath());
            }
            if (isSubstringSearchRequested()) {
                File[] dirContents = dir.listFiles();
                for (File f : dirContents) {
                    if (f.getName().toLowerCase().contains(lowerCmd)) {
                        matches.add(f);
                    }
                }
            }
            else {
                for (String ext : pathExt) {
                    File file = new File(dir, cmd + ext);
                    if (file.isFile()) {
                        matches.add(file);
                        if (! findAllRequested) {
                            break;
                        }
                    }
                }
            }
        }
        return matches;
    }

    protected void help() {
        System.out.println("Displays the location of the program that will be executed ");
        System.out.println("when the programname is typed at the command line.");
        System.out.println("The PATH is searched in order for a matching program.");
        System.out.println("PATHEXT is used if it is available.");
        System.out.println();
        System.out.println("WHICH [/A] [/S] [/V] programname ...");
        System.out.println();
        System.out.println("   /A   Display all mathing files in PATH");
        System.out.println("   /S   Substring search of file names in PATH directories");
        System.out.println("   /V   Verbose output");
        System.out.println();
        System.out.println("Returns 0 on success or 1 on failure.");
    }

}

