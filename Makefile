
all: jarget.jar

sh: bin/jarget.sh 

SCALA_XML=/home/archbox/opt/scala-2.12.3/lib/scala-xml_2.12-1.0.6.jar

jarget.jar : jarget.scala 
	scalac jarget.scala -d jarget.jar

bin/jarget-fat.jar: jarget.jar 
	jar-tools.sh -scala-build-jar bin/jarget-fat.jar jarget.jar $(SCALA_XML)

bin/jarget.sh: bin/jarget-fat.jar
	jar-tools.sh -jar-to-sh bin/jarget-fat.jar bin/jarget.sh

clean:
	rm -rf jarget.jar 

test1:
	echo "Downloading JFreechart"
	scala jarget.jar -get org.jfree/jfreechart/1.0.19 
