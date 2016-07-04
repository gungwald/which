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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.accumulatorx.os.Environment;
import com.accumulatorx.os.EnvironmentFactory;

public class Which {
    
    public static final int EXIT_SUCCESS = 0;
    public static final int EXIT_FAILURE = 1;
    
    public Environment env = null;
    
    private List<String> path = null;
    private List<String> pathExt = null;
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
        env = new EnvironmentFactory().getEnvironment();
        pathExt = env.getMultiValue("PATHEXT");
        path = env.getMultiValue("PATH");
        if (path == null) {
            System.err.println("PATH environment variable is not set.");
            System.exit(EXIT_FAILURE);
        }

        if (env.isWindows()) {
            // Add the assumed current directory to the path.
            path = push(path, System.getProperty("user.dir"));
        }
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

    public ArrayList<File> find(String cmd) throws IOException {
        ArrayList<File> matches = new ArrayList<File>();
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

