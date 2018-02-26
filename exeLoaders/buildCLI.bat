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

rem Compile using MSVC or Visual C++ Compiler.         
cl.exe -Zi loader.cpp

    
      
