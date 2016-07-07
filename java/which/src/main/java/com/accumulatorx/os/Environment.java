package com.accumulatorx.os;

import java.io.IOException;
import java.util.List;

public interface Environment {

    public String get(String name) throws IOException;
    public List<String> getMultiValue(String name) throws IOException;
    public boolean isWindows();
}
