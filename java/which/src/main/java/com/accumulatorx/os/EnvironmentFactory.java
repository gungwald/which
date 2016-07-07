package com.accumulatorx.os;

import java.io.IOException;

public class EnvironmentFactory {
    
	public Environment getEnvironment() throws IOException {
		return new ShellEnvironment();
	}

}
