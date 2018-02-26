# Makefile to build jarget. 
#
# Rules: 
# 
# $ make check		-> Print make file variables to check them.
#
# $ make			-> Build jarget.jar app runnable with '$ scala jarget.jar' (not self-contained).
#
# $ make force      -> Build jarget.jar forcing compilation without the fsc daemon.
#
# $ make sh			-> Build bin/jarget - uber jar file for testing.
#
# $ make sh-guard	-> Build bin/jarget - uber jar shrunk with proguard. 
#
# $ make install    -> Copy file bin/jarget to ~/bin/jarget or $HOME/bin/jarget 
#
#
#============================ V A R I A B L E S =======================# 

target   := jarget.jar 
#sh-guard := bin/jarget  

# Name of executable uber-jar file not shrunk by proguard.
sh   := bin/jarget

fatjar :=  bin/jarget-uber.jar

# Scala source files necessary to build the project
src			:= $(wildcard src/*.scala)

# Assets/assets files to be bundled with the uber-jar 
assetfiles	:= $(wildcard assets/*)

SCALA_XML=scala-xml_2.12-1.0.6.jar
SCALA_XML_PATH=$(shell dirname $(shell dirname $(shell which scala)))/lib/$(SCALA_XML)

exeloaders := exeLoaders/loaderCLI.exe exeLoaders/loaderGUI.exe

#=============================  R U L E S =================================#

all:      $(target)
sh:       $(sh)
sh-quard: $(sh-guard)

# This rule checks make variables 
check:
	@echo "src             = "$(src)
	@echo "assetfiles      = "$(assetfiles)
	@echo "SCALA_XML_PATH  = "$(SCALA_XML_PATH)


$(target) : $(src)
	fsc $(src) -d jarget.jar

force: $(src)
	scalac $(src) -d jarget.jar 

bin/jarget: $(target) $(assetfiles)
	mkdir -p bin
	cp -v $(exeloaders) assets || true 
	scala jarget.jar uber -scala -sh -o $(sh) -m jarget.jar -j $(SCALA_XML_PATH) -r assets

$(fatjar): 
	mkdir -p bin
	@# Try to copy Windows CLI and GUI Loaders	
	cp -v $(exeloaders) assets || true 
	scala jarget.jar uber -scala -o bin/jarget-uber.jar -m jarget.jar -j $(SCALA_XML_PATH) -r assets

# Generates files bin/jarget shrunk with proguard
#
sh-guard: $(target) $(assetfiles) config.pro
	mkdir -p bin
	@# Try to copy Windows CLI and GUI Loaders
	cp -v $(exeloaders) assets || true 
	@# Generate uber jar
	scala jarget.jar uber -scala -o bin/jarget-uber.jar -m jarget.jar -j $(SCALA_XML_PATH) -r assets 
	@# Shrink app with proguard 
	java -jar proguard.jar @config.pro
	@# Make file executable 
	scala jarget.jar uber -exjar bin/jarget-pro.jar bin/jarget
	@# Remove temporary files
	rm -rf bin/jarget-uber.jar bin/jarget-pro.jar

install: bin/jarget
	cp -v bin/jarget ~/bin

clean:
	rm -rf jarget.jar bin/jarget bin/jarget-fat.jar

upload: bin/jarget
	cd bin && git add jarget && git commit -a -m "Update release" && git push 

doc: $(src)
	scaladoc $(src) -author -doc-title "Jarget - Scala/Java automation tool" -doc-version "1.0" -doc-source-url "https://github.com/caiorss/jarget" -d ./bin/docs 

doc-upload: $(src) jarget.jar 
	scaladoc $(src) -author -doc-title "Jarget - Scala/Java automation tool" -doc-version "1.0" -doc-source-url "https://github.com/caiorss/jarget" -d ./bin/docs
	cd bin && git add docs/ && git commit -a -m "Update docs" && git push 

page:
	cp README.html bin/index.html
	cp -r -v images   bin/images
	cp -r -v scripts  bin/scripts
	cd bin && git add images scripts 
	cd bin && git commit -a -m "Update project website" && git push 

tags: $(src)
	ctags src/*.scala 

# -------- Test Commands ----------- #


test1: jarget.jar 
	echo "Downloading JFreechart"
	scala jarget.jar -get org.jfree/jfreechart/1.0.19 

test2: jarget.jar 
	scala jarget.jar -get org.jmdns/jmdns/3.5.1
