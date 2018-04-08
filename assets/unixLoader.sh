#!/usr/bin/env sh

# Check if JAVA_HOME is Set 
if [ -n "${JAVA_HOME}" ]
then
    # Check if JAVA is Installed in this JAVA_HOME
    if [ -f  "$JAVA_HOME/bin/java" ] ;
    then
        "$JAVA_HOME/bin/java" -jar "$0" "$@"
    # Try to use JAVA from $PATH Variable
    else
        # Check if Java is Installed 
        if hash java 2>/dev/null;
        then
            java -jar "$0" "$@"
        else
            echo "Error: Java not available in PATH variable or in \$JAVA_HOME"
            echo "Make sure Java is installed"
            exit 1
        fi 
    fi 
else
    # Check if Java is Installed 
    if hash java 2>/dev/null;
    then
        java -jar "$0" "$@"
    else
        echo "Error: Java not available in PATH variable"
        echo "Make sure Java is installed"
        exit 1
    fi     
fi
exit 0
