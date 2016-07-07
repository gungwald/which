package com.accumulatorx.os;
public class PlainPosix
{
    /**
     * Retrieves an environment variable value.
     * 
     * @param name  The name of the environment variable
     * @return      The value of the environment variable
     */
    public static native String getenv(String name);
}

