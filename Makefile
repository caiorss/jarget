all:       jarget.jar
sh:        bin/jarget 
sh-guard:  

fatjar :=  bin/jarget-uber.jar

src := src/jarget.main.scala src/jarget.utils.scala src/jarget.mvn.scala src/jarget.crypto.scala src/jarget.logger.scala

SCALA_XML=scala-xml_2.12-1.0.6.jar
SCALA_XML_PATH=$(shell dirname $(shell dirname $(shell which scala)))/lib/$(SCALA_XML)

jarget.jar : $(src)
	fsc $(src) -d jarget.jar

bin/jarget: jarget.jar assets/version.txt assets/user-help.txt
	scala jarget.jar uber -scala -sh -o bin/jarget -m jarget.jar -j $(SCALA_XML_PATH) -r assets

$(fatjar): jarget.jar assets/version.txt assets/user-help.txt
	scala jarget.jar uber -scala -o bin/jarget-uber.jar -m jarget.jar -j $(SCALA_XML_PATH) -r assets

sh-guard: $(fatjar) config.pro
	echo $(fatjar)
	java -jar proguard.jar @config.pro
	scala jarget.jar uber -exjar bin/jarget-pro.jar bin/jarget

install: bin/jarget
	cp -v bin/jarget ~/bin

clean:
	rm -rf jarget.jar bin/jarget bin/jarget-fat.jar

upload: bin/jarget
	cd bin && git add jarget && git commit -a -m "Update release" && git push 

doc: $(src)
	scaladoc $(src) -author -doc-title "Jarget - Scala/Java automation tool" -doc-version "1.0" -doc-source-url "https://github.com/caiorss/jarget" -d ./bin/docs 

doc-upload: $(src)
	scaladoc $(src) -author -doc-title "Jarget - Scala/Java automation tool" -doc-version "1.0" -doc-source-url "https://github.com/caiorss/jarget" -d ./bin/docs
	cd bin && git add docs/ && git commit -a -m "Update docs" && git push 


# -------- Test Commands ----------- #


test1: jarget.jar 
	echo "Downloading JFreechart"
	scala jarget.jar -get org.jfree/jfreechart/1.0.19 

test2: jarget.jar 
	scala jarget.jar -get org.jmdns/jmdns/3.5.1
