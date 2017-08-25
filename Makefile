
src := src/jarget.main.scala src/jarget.utils.scala src/jarget.mvn.scala 


all: jarget.jar

sh: bin/jarget 

fatjar: bin/jarget-fat.jar


SCALA_XML=scala-xml_2.12-1.0.6.jar
SCALA_XML_PATH=$(shell dirname $(shell dirname $(shell which scala)))/lib/$(SCALA_XML)

jarget.jar : $(src)
	fsc $(src) -d jarget.jar

bin/jarget: jarget.jar
	echo $(SCALA_XML)
	scala jarget.jar uber -scala -sh -o bin/jarget -m jarget.jar -j $(SCALA_XML_PATH)

install: bin/jarget
	cp -v bin/jarget ~/bin

clean:
	rm -rf jarget.jar bin/jarget bin/jarget-fat.jar

upload: bin/jarget
	cd bin && git add jarget && git commit -a -m "Update release" && git push 

# -------- Test Commands ----------- #


test1: jarget.jar 
	echo "Downloading JFreechart"
	scala jarget.jar -get org.jfree/jfreechart/1.0.19 

test2: jarget.jar 
	scala jarget.jar -get org.jmdns/jmdns/3.5.1
