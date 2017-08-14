
all: jarget.jar

sh: bin/jarget 

fatjar: bin/jarget-fat.jar

SCALA_XML=/home/archbox/opt/scala-2.12.3/lib/scala-xml_2.12-1.0.6.jar

jar-tools.sh:
	curl -O -L https://raw.githubusercontent.com/caiorss/build-fat-jar/master/jar-tools.sh
	chmod +x jar-tools.sh

jarget.jar : jar-tools.sh jarget.scala 
	scalac jarget.scala -d jarget.jar

bin/jarget-fat.jar: jar-tools.sh jarget.jar 
	./jar-tools.sh -scala-build-jar bin/jarget-fat.jar jarget.jar $(SCALA_XML)

bin/jarget: bin/jarget-fat.jar
	./jar-tools.sh -jar-to-sh bin/jarget-fat.jar bin/jarget

install: bin/jarget
	cp -v bin/jarget ~/bin

clean:
	rm -rf bin/jarget bin/jarget-fat.jar

# -------- Test Commands ----------- #


test1: jarget.jar 
	echo "Downloading JFreechart"
	scala jarget.jar -get org.jfree/jfreechart/1.0.19 

test2: jarget.jar 
	scala jarget.jar -get org.jmdns/jmdns/3.5.1
