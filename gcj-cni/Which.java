/*
 * which - Find a command in the PATH in Windows
 * Copyright (c) 2002 Bill Chatfield
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

import java.lang.System;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Vector;
import java.util.StringTokenizer;

public class Which
{
    protected String[] path = null;
    protected String[] pathExt = {".COM", ".EXE", ".BAT"};
    protected boolean verbose = false;

    public Which(String path, String pathExt)
    {
        this.path = parse(path, System.getProperty("path.separator"));
        this.pathExt = parse(pathExt, ";");  // Cygwin does not change this
    }

    public Which(String path)
    {
        this.path = parse(path, System.getProperty("path.separator"));
    }

    public void setVerbose(boolean value)
    {
        verbose = value;
    }

    public boolean isVerbose()
    {
        return verbose;
    }

    protected String[] parse(String path, String pathSeparator)
    {
        Vector pathList = new Vector();
        StringTokenizer pathTokenizer = new StringTokenizer(path, pathSeparator);
        while (pathTokenizer.hasMoreTokens())
        {
            String pathElement = pathTokenizer.nextToken();
            pathList.addElement(pathElement);
        }
        String[] paths = new String[pathList.size()];
        pathList.copyInto(paths);
        return paths;
    }

    public File correctCase(File file) {
        File parentDirectory = file.getParentFile();
        FixFilenameFilter filter = new FixFilenameFilter(file.getName());
        File[] files = parentDirectory.listFiles(filter);
        return files[0];  // Only one should match my filter
    }

    public String findFirst(String cmd) throws IOException
    {
        String fileSeparator = System.getProperty("file.separator");
        for (int i = 0; i < path.length; i++)
        {
            for (int j = 0; j < pathExt.length; j++)
            {
                File file = new File(path[i] + fileSeparator + cmd + pathExt[j]);
                if (verbose)
                    System.out.print("   Trying " + file.getAbsolutePath() + ": ");
                if (file.isFile())
                {
                    if (verbose) {
                        System.out.println("THIS IS A MATCH");
                    }
                    return correctCase(file).getCanonicalPath();
                }
                else
                {
                    if (verbose)
                        System.out.println("no match");
                }
            }
        }
        return null;
    }

    public String[] findAll(String cmd) throws IOException
    {
        String[] allOfThem = null;
        Vector locations = new Vector();
        String fileSeparator = System.getProperty("file.separator");
        for (int i = 0; i < path.length; i++)
        {
            for (int j = 0; j < pathExt.length; j++)
            {
                File file = new File(path[i] + fileSeparator + cmd + pathExt[j]);
                if (verbose)
                    System.out.print("   Trying " + file.getAbsolutePath() + ": ");
                if (file.isFile())
                {
                    if (verbose) {
                        System.out.println("THIS IS A MATCH");
                    }
                    locations.addElement(correctCase(file).getCanonicalPath());
                }
                else
                {
                    if (verbose)
                        System.out.println("no match");
                }
            }
        }
        int size = locations.size();
        if (size > 0)
        {
            allOfThem = new String[size];
            locations.copyInto(allOfThem);
        }
        return allOfThem;
    }

    protected static String getEnvironmentVariable(String name)
    {
        return Posix.getenv(name);
    }

    protected static void help()
    {
        System.out.println("Displays the location of the program that will be executed ");
        System.out.println("when the programname is typed at the command line.");
        System.out.println("The PATH is searched in order for a matching program.");
        System.out.println("PATHEXT is used if it is available.");
        System.out.println();
        System.out.println("WHICH [/A] [/V] programname ...");
        System.out.println();
        System.out.println("   /A   Display all mathing files in PATH");
        System.out.println("   /V   Verbose output");
        System.out.println();
        System.out.println("Returns the number of failed matches or -1 if an error");
        System.out.println("occurs.");
    }

    public static void main(String[] args)
    {
        int numberFailed = 0;
        try
        {
            Which which = null;
            if (args.length < 1) {
                System.out.println("usage: which [program name]");
                System.exit( -1);
            }
            String path = getEnvironmentVariable("PATH");
            if (path == null) {
                System.err.println("PATH environment variable is not set.");
                System.exit( -1);
            }
            // Add the assumed current directory to the path.
            String cwd = System.getProperty("user.dir");
            String pathSeparator = System.getProperty("path.separator");
            path = cwd + pathSeparator + path;
            String pathExt = getEnvironmentVariable("PATHEXT");
            if (pathExt == null) {
                which = new Which(path);
            } else {
                which = new Which(path, pathExt);
            }
            int argc = args.length;
            Vector commands = new Vector();
            boolean findThemAll = false;
            for (int i = 0; i < argc; i++) {
                String arg = args[i];
                if (arg.equalsIgnoreCase("/v") || arg.equalsIgnoreCase("-v")) {
                    which.setVerbose(true);
                } else if (arg.equalsIgnoreCase("/a") || arg.equalsIgnoreCase("-a")) {
                    findThemAll = true;
                } else if (arg.equalsIgnoreCase("/?")
                           || arg.equalsIgnoreCase("/h")
                           || arg.equalsIgnoreCase("/help")
                           || arg.equalsIgnoreCase("--help")
                           || arg.equalsIgnoreCase("-help")
                           || arg.equalsIgnoreCase("-h")
                           || arg.equalsIgnoreCase("-?")) {
                    help();
                } else if (arg.startsWith("/") || arg.startsWith("-")) {
                    System.err.println("Invalid switch: " + arg);
                    System.exit( -1);
                } else {
                    commands.addElement(arg);
                }
            }
            int size = commands.size();
            for (int i = 0; i < size; i++) {
                String cmd = (String) commands.elementAt(i);
                if (findThemAll) {
                    String[] locations = which.findAll(cmd);
                    if (locations == null) {
                        numberFailed++;
                        System.out.println(cmd + " not found in PATH.");
                    } else {
                        int length = locations.length;
                        for (int j = 0; j < length; j++)
                            System.out.println(locations[j]);
                    }
                } else {
                    String location = which.findFirst(cmd);
                    if (location == null) {
                        numberFailed++;
                        System.out.println(cmd + " not found in PATH.");
                    } else
                        System.out.println(location);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            numberFailed = 1;
        }
        System.exit(numberFailed);
    }

    protected class FixFilenameFilter implements FilenameFilter {
        String name = null;
        public FixFilenameFilter(String name) {
            this.name = name;
        }
        public boolean accept(File dir, String name) {
            boolean result = false;
            if (this.name.equalsIgnoreCase(name)) {
                result = true;
            }
            return result;
        }
    }
}

