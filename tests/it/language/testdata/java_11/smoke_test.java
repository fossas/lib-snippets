import java.util.HashMap;
import java.util.logging.Logger;

public class TestFunctions {

    // Logger for logging messages
    private static final Logger logger = Logger.getLogger(TestFunctions.class.getName());

    // Default constructor
    public TestFunctions() {
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

    // Static method
    public static void staticMethod() {
        logger.info("staticMethod called");
    }

    // Private method
    private void privateMethod() {
        logger.info("privateMethod called");
        protectedMethod(); // Calling a protected method
    }

    // Protected method
    protected void protectedMethod() {
        logger.info("protectedMethod called");
    }

    // Method with a complex parameter
    public void methodWithComplexParam(HashMap<String, Integer> map) {
        logger.info("methodWithComplexParam called");
        privateMethod(); // Calling a private method
    }

    // Method with varargs
    public void methodWithVarargs(String... args) {
        logger.info("methodWithVarargs called with " + args.length + " arguments");
        // Calling a method with a complex parameter
        HashMap<String, Integer> map = new HashMap<>();
        map.put("key", 1);
        methodWithComplexParam(map);
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
