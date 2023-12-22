package com.example.MyApp;

import java.util.HashMap;
import java.util.logging.Logger;

// Simpler form of smoke test, but with a non-default package.
public class AppFunctions {

    // Logger for logging messages
    private static final Logger logger = Logger.getLogger(TestFunctions.class.getName());

    // Default constructor
    public AppFunctions() {
        logger.info("Constructor called");
    }

    // Method with no return type and no parameters
    public void simpleMethod() {
        logger.info("simpleMethod called");
        methodWithParam(5); // Calling another method
    }

    // Method with a return type and one parameter
    public int methodWithParam(int a) {
        logger.info("methodWithParam(int a) called with a = " + a);
        return a;
    }

    // Overloaded method with two parameters
    public int methodWithParam(int a, int b) {
        logger.info("methodWithParam(int a, int b) called with a = " + a + ", b = " + b);
        staticMethod(); // Calling a static method
        return a + b;
    }

    // Inner class with a constructor
    class InnerClass {
        public InnerClass() {
            logger.info("InnerClass constructor called");
        }
    }

    // Static inner class with a constructor
    static class StaticInnerClass {
        public StaticInnerClass() {
            logger.info("StaticInnerClass constructor called");
        }
    }
}
