-injars bin/jarget-uber.jar 
# -injars  /home/archbox/opt/scala-2.12.3/lib/scala-library.jar
# -injars  /home/archbox/opt/scala-2.12.3/lib/scala-xml_2.12-1.0.6.jar  
-outjars bin/jarget-pro.jar
-libraryjars <java.home>/lib/rt.jar

-dontwarn scala.** -verbose

# -dontshrink
-dontoptimize
-dontobfuscate

# -keepattributes Signature


########################################

-keepclasseswithmembers public class * {
   public static void main(java.lang.String[]);
}

-keepattributes Signature
-keepattributes *Annotation*
-keepattributes InnerClasses
-keepattributes SourceFile
-keepattributes LineNumberTable

-keep interface java8.** { *; }
-keep class java8.** { *; }
# -keep class java8.** { *; }
# -dontobfuscate  java8.**
# -dontshrink  java8.**

-adaptresourcefilecontents  assets/version.txt,assets/user-help.txt
