/* Wide characters are used exclusively because Windows uses them internally
   so they are faster than chars because there is no translation.

   C++ is intentionally being avoided so as to avoid a dependency on the C++
   standard library, MSVCPXX.DLL (Microsoft) or libstdc++.dll (MinGW). These
   would either have to be distributed with the exe or would have to already
   exist on all target machines. And if it were assumed to exist on the target
   machines, then there is the problem of having the right version. This is
   just another potential point of failure.

   The C++ standard library could be statically linked, but this would cause
   the exe to be gigantic, which is also undesirable and unnecessary.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <direct.h>     /* _getcwd */
#include <windows.h>
#include <wchar.h>

#include "stringconcat.h"
#include "errors.h"

/* Constants */
const wchar_t *PATH = L"PATH";
const wchar_t *PATHEXT = L"PATHEXT";
const wchar_t *SEMICOLON = L";";

/* Types */
struct options 
{
    bool findallmatches;
};

/* Functions */
bool which(wchar_t * filename, struct options *opt);
bool whichext(const wchar_t *filename, const wchar_t *ext, const wchar_t *path);
void usage();

/* Global variables */
wchar_t *progname;

#include "main.h"

/* Main Program */
int wmain(int argc, wchar_t *argv[])
{
    int i;
    struct options opt;
    progname = argv[0];

    opt.findallmatches = false;
    
    if (argc > 1) 
    {
        for (i = 1; i < argc; i++) 
        { 
            if (wcscmp(argv[i], L"-a") == 0 || wcscmp(argv[i], L"/a") == 0)
            {
                opt.findallmatches = true;
            }
            else
            {
                which(argv[i], &opt);
            }
        }
    } 
    else
    {
        usage();
    }
    return EXIT_SUCCESS;
}

bool which(wchar_t * filename, struct options *opt)
{
    wchar_t * path;
    wchar_t * pathext;
    wchar_t * ext;
    bool foundmatch = false;
    bool foundatleastone = false;
    wchar_t *cwd;

    path = _wgetenv(PATH);
    
    if (path != NULL)
    {
        /* We need to add the current working directory to the PATH because
           Windows looks there first. */
        if ((cwd = _wgetcwd(NULL, 0)) != NULL) {
            path = concat3(cwd, L";", path);
            pathext = _wgetenv(PATHEXT);

            if (pathext != NULL) {
                /* The pathext value returned by getenv is not modifiable. So it is
                   necessary to create a copy which is modifiable, using _strdup. 
                   This will have to be freed when we are done with it. The strtok
                   function requires a modifiable variable. */
                pathext = _wcsdup(pathext);
    
                ext = wcstok(pathext, SEMICOLON);
                while (ext != NULL && (opt->findallmatches || !foundmatch)) {
                    foundmatch = whichext(filename, ext, path);
                    if (foundmatch) {
                        foundatleastone = true;
                    }
                    ext = wcstok(NULL, SEMICOLON);
                }

                if (! foundatleastone) {
                    wprintf(L"'%s' was not found in PATH using PATHEXT extensions.\n", filename);
                }
                free(pathext);
            }
            else {
                fwprintf(stderr, L"Your PATHEXT environment variable is not set.\n");
            }
            free(path);
        }
        else {
            _wperror(L"Failed to get current directory");
        }
    }
    else {
        fwprintf(stderr, L"Your PATH environment variable is not set.\n");
    }
    return foundatleastone;
}

bool whichext(const wchar_t *searchfile, const wchar_t *ext, const wchar_t *path)
{
    wchar_t filefoundinpath[MAX_PATH + 1];
    LPTSTR nameptr;
    DWORD filefoundlen;
    DWORD errorcode;
    WIN32_FIND_DATA filedetails;
    HANDLE findhandle;
    bool foundfile = false;
    wchar_t drive[_MAX_DRIVE+1];
    wchar_t dir[_MAX_DIR+1];

    filefoundlen = SearchPath(path, searchfile, ext, MAX_PATH, filefoundinpath, &nameptr);

    if (filefoundlen > 0) 
    {
        /* SearchPath does not correct any characters in the searchfile
           which were given in the wrong case. So, we have to do FindFirstFile
           to correct this. */
        findhandle = FindFirstFile(filefoundinpath, &filedetails);
        if (findhandle != INVALID_HANDLE_VALUE)
        {
            /* _splitpath is non-standard but quite useful on Windows. 
               Microsoft defines it in stdlib.h. */
            _wsplitpath(filefoundinpath, drive, dir, NULL, NULL);
            wprintf(L"%s%s%s\n", drive, dir, filedetails.cFileName);

            foundfile = true;
            FindClose(findhandle);
        }
        else
        {
            printerror(filefoundinpath, GetLastError());
        }
    }
    else 
    {
        if ((errorcode = GetLastError()) != ERROR_FILE_NOT_FOUND)
        {
            printerror(searchfile, errorcode);
        }
    }
    return foundfile;
}

void usage()
{
    _putws(L"usage: which [-a] command...");
}
