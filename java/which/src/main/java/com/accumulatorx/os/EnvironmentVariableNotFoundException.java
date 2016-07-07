package com.accumulatorx.os;

public class EnvironmentVariableNotFoundException extends Exception {

	private static final long serialVersionUID = 1L;
    
	private String environmentVariableName;

	public EnvironmentVariableNotFoundException(String environmentVariableName) {
		super("Environment variable not found: " + environmentVariableName);
        this.environmentVariableName = environmentVariableName;
	}

	public EnvironmentVariableNotFoundException(String environmentVariableName, Throwable cause) {
		super("Environment variable not found: " + environmentVariableName, cause);
        this.environmentVariableName = environmentVariableName;
	}

	/**
	 * @return the environmentVariableName
	 */
	public String getEnvironmentVariableName() {
		return environmentVariableName;
	}

}
