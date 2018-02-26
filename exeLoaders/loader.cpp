#include<iostream>
#include<string>
#include<vector>

#if defined  __linux__ || __apple__
#pragma message("Compiling for Unix")
#include <stdlib.h>
#include <limits.h>  // PATH_MAX
#include <unistd.h>
#include <sys/stat.h>   // mkdir
// #include <stdio.h>      // perror
#endif

#ifdef _WIN32
#define UNICODE
#define _UNICODE
#include <windows.h>  // Windows Kernel API
#include <tchar.h>
#include <wchar.h>
#include <io.h>
#endif

using namespace std;

enum class SystemType{
	WindowsNT,
	Linux,
	MacOSX,
	FreeBSD,
	Unknown
};

void errorNotImplemented(){
	throw "Error: not implemented";
	// exit(1);
}

//---------- Windows Helpers ------------//

#ifdef _WIN32
#pragma message("Compiling for Windows")

/** This namespace provides basic Windows utilities */
namespace winUtils{

	// Credits: https://www.chilkatsoft.com/p/p_348.asp
	// 65001 is utf-8.
	wchar_t *CodePageToUnicode(const char *src, int codePage = 65001){
		if (!src) return 0;
		int srcLen = strlen(src);
		if (!srcLen)
		{
		wchar_t *w = new wchar_t[1];
		w[0] = 0;
		return w;
		}

		int requiredSize = MultiByteToWideChar(codePage,
			0,
			src,srcLen,0,0);

		if (!requiredSize)
			{
			return 0;
			}

		wchar_t *w = new wchar_t[requiredSize+1];
		w[requiredSize] = 0;

		int retval = MultiByteToWideChar(codePage,
			0,
			src,srcLen,w,requiredSize);
		if (!retval)
			{
			delete [] w;
			return 0;
			}

		return w;
	}

	// Credits - https://www.chilkatsoft.com/p/p_348.asp
	char* UnicodeToCodePage(const wchar_t *src, int codePage = 1252){
		if (!src) return 0;
		int srcLen = wcslen(src);
		if (!srcLen)
		{
		char *x = new char[1];
		x[0] = '\0';
		return x;
		}
		int requiredSize = WideCharToMultiByte(codePage,
			0,
			src,srcLen,0,0,0,0);

		if (!requiredSize){
			return 0;
		}
		char *x = new char[requiredSize+1];
		x[requiredSize] = 0;
		int retval = WideCharToMultiByte(codePage,
			0,
			src,srcLen,x,requiredSize,0,0);
		if (!retval)
			{
			delete [] x;
			return 0;
			}

		return x;
	}


	LPWSTR stringToLPWSTR(string text){
		// wchar_t* wtext = new wchar_t[text.size()];
		// mbstowcs(wtext, text.c_str(), strlen(text.c_str()) + 1);
		// return wtext;
		return CodePageToUnicode(text.c_str(), 1252);
	}

	string lpwstrToString(wchar_t* wtext){
		char* str = UnicodeToCodePage(wtext, 1252);
		string s = string(str);
		delete [] str;
		return s;
	}

}; // -- End of namespace WinUtils ---//

#endif

//------------ Cross Platform Codes ---------//

string getEnv(std::string var){
	#if defined __linux__ || __apple__
	return string(getenv(var.c_str()));
	#elif defined _WIN32
	// Get required buffer size
	static const int MAX_ENV = 1024;
	wchar_t* varp = winUtils::stringToLPWSTR(var);
	DWORD size = GetEnvironmentVariable(varp, NULL, 0);
	wchar_t env[MAX_ENV];
	GetEnvironmentVariable(varp, env, MAX_ENV);
	delete [] varp;
	return winUtils::lpwstrToString(env);
	#else
	#error "Unknown platform"
	#endif
}


/**

   Documentation:
   -  CreateDirectoryW(LPCWSTR, LPSECURITY_ATTRIBUTES)
   -CreateDirectory -  https://msdn.microsoft.com/en-us/library/windows/desktop/aa363855(v=vs.85).aspx

 */
void makeDirectory(std::string path){
	#if defined __linux__ || defined __apple__
	mkdir(path.c_str(), 0777);
	#elif _WIN32
	wchar_t* wtext = winUtils::stringToLPWSTR(path);
	CreateDirectoryW(wtext, NULL);
	delete [] wtext;
	#endif
}


/**  Returns the current operating system that the library was compiled against.
*/
SystemType getSystemType(){
	#ifdef __apple__
	return SystemType::MacOSX;
	#elif defined __linux__
	return SystemType::Linux;
	#elif defined _WIN32 || defined _WIN64
	return SystemType::WindowsNT;
	#else
    return SystemType::Unknown;
	#endif
}

string getOperatingSystem(){
	SystemType type = getSystemType();
	if (type == SystemType::Linux)
		return "Linux";
	else if(type == SystemType::MacOSX)
		return "MacOSX";
	else if(type == SystemType::WindowsNT)
		return "Windows NT";
	else if(type == SystemType::FreeBSD)
		return "FreeBSD";
	else
		return "Unknown operating system";
}

string getHomeDir(){
	if (getSystemType() == SystemType::WindowsNT)
	 	return getEnv("USERPROFILE");
	 else
		 return getEnv("HOME");
}


/**  Get path of current executable
 */
string getExecutablePath(){
  #if defined __linux__
  char buffer [PATH_MAX];
  ssize_t len = readlink("/proc/self/exe", buffer, PATH_MAX-1);
  if (len != -1){
	  buffer[len] = '\0';
	  return string(buffer);
  } else
	  return "";
  #elif defined _WIN32
  HMODULE hModule = GetModuleHandleW(NULL);
  WCHAR wpath[MAX_PATH];
  GetModuleFileNameW(hModule, wpath, MAX_PATH);
  wstring ws(wpath);
  string str(ws.begin(), ws.end());
  return str;
  #else
  #error "Unknown platform"
  #endif
}

bool fileExists(string path){
	// Unix:    include <unistd.h>
	// Windows: include <windows.h> and <io.h>
	return access(path.c_str(), 0) == 0;
}

void execProc(string program, vector<string> args, bool console = true){
  #if defined __linux__ || defined __apple__ 
	const char **argsarr = new const char* [args.size() + 2];
	argsarr[0] = "program"; // Set program name
	for(int i = 0 ; i < args.size(); i++){
		argsarr[i+1] = args[i].c_str();
	}
	argsarr[args.size() + 1] = NULL;
	int status = execvp(program.c_str(), (char **) argsarr);
	delete [] argsarr;
	// return status;
  #elif defined _WIN32

	STARTUPINFO si = {0,};
	PROCESS_INFORMATION pi;
	si.cb = sizeof(si);
	si.lpTitle = _T("program");
	string command = program;

	for (auto arg = args.begin(); arg != args.end(); arg++ ){
		command = command + " " + *arg;
	}

	bool state;

	LPWSTR cmdwstr = winUtils::stringToLPWSTR(command);

	if (!console){
		// cout << "Running process = " << command << endl;
		state = CreateProcess(
			// No module name (use command line)
			NULL,
			// Command line
			cmdwstr, //  winUtils::stringToLPWSTR(command.c_str()),
			// Process handle not inheritable
			NULL,
			// Thread handle not inheritable
			NULL,
			// Set handle inheritance to FALSE
			FALSE,
			// No creation flags
			CREATE_NO_WINDOW,
			// Use parent's environment block
			NULL,
			// Use parent's starting directory
			NULL,
			// Pointer to STARTUPINFO structure
			&si,
			// Pointer to PROCESS_INFORMATION structure
			&pi
			);

		delete [] cmdwstr;
	} else{
		cmdwstr = winUtils::stringToLPWSTR(command);
		state = CreateProcess(
			// No module name (use command line)
			NULL,
			// Command line
			cmdwstr,
			// Process handle not inheritable
			NULL,
			// Thread handle not inheritable
			NULL,
			// Set handle inheritance to FALSE
			TRUE,
			// No creation flags
			0,
			// Use parent's environment block
			NULL,
			// Use parent's starting directory
			NULL,
			// Pointer to STARTUPINFO structure
			&si,
			// Pointer to PROCESS_INFORMATION structure
			&pi
			);

		delete [] cmdwstr;

	}
	// Wait until child process exits.
	WaitForSingleObject( pi.hProcess, INFINITE );

	CloseHandle( pi.hProcess );
	CloseHandle( pi.hThread );

  #else
  #error "Unknown platform"
  #endif
}

/** Experimental: Doesn't work quite right yet. */ 
void xdgOpen(string file){
	SystemType os = getSystemType();
	if (os == SystemType::Linux || os == SystemType::FreeBSD)
		execProc("xdg-open", {"'" + file + "'"});
	else if (os == SystemType::MacOSX)
		execProc("open", {file});
	else if (os == SystemType::WindowsNT)
		execProc("cmd", {"/C", "start", "", file}, true);
	else {
		cerr << "Error: unknown operating system";
		exit(1);
	}
}

int main(int argn, char** argv){

	vector<string> pargs ;
	pargs.push_back("-jar");
	pargs.push_back(getExecutablePath());

	for (int i = 1; i < argn; i++){
		pargs.push_back(argv[i]);
	}
	
	execProc("java", pargs, true);
	
	return 0;
}
