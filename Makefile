
all: jarget.jar 

SCALA_XML=/home/archbox/opt/scala-2.12.3/lib/scala-xml_2.12-1.0.6.jar

jarget.jar : jarget.scala 
	scalac jarget.scala -d jarget.jar

fatjar: jarget.jar 
	jar-tools.sh -scala-build-jar bin/jarget-fat.jar jarget.jar $(SCALA_XML)

clean:
	rm -rf jarget.jar 

test1:
	echo "Downloading JFreechart"
	scala jarget.jar -get org.jfree/jfreechart/1.0.19 
