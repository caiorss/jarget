@echo off
rem  Script to build C++ programs with MSVC compiler or CL.EXE
rem  with Visual Studio 2017 tools 
rem  -    
rem 


rem Save current directory     
pushd %CD%

rem  Compile for x86 or x64 bits 
rem ------------------------------
set MODE=x86          
rem set MODE=x64


call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsamd64_x86.bat" %MODE%

rem Restore saved directory     
popd 

rem Build CLI - exe loader for Java (CLI) Command Line Applications 
cl.exe loader.cpp /Fe:loaderCLI.exe 

rem Build CLI - exe loader for Java (GUI) - Graphical User Interfaces
set LINK=/entry:mainCRTStartup  
cl.exe loader.cpp /DGUIAPP /link user32.lib /subsystem:windows /out:loaderGUI.exe 
    
rem cl.exe -Zi loader2.cpp    

    
      
