#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <direct.h>     /* _getcwd */
#include <windows.h>
#include <lmerr.h>

/* Constants */
LPCTSTR PATH = "PATH";
LPCTSTR PATHEXT = "PATHEXT";
LPCTSTR SEMICOLON = ";";

/* Types */
struct options 
{
    bool findallmatches;
};

/* Functions */
bool which(LPCTSTR filename, struct options *opt);
bool whichext(const TCHAR *filename, const TCHAR *ext, const TCHAR *path);
void usage();
TCHAR *errortext(DWORD errorcode);
void printerror(const TCHAR *offendingobject, DWORD errorcode);
TCHAR *concat(const TCHAR *s, const TCHAR *t);
TCHAR *concatdword(const TCHAR *s, DWORD n);

/* Global variables */
char *progname;


/* Main Program */
int main(int argc, char *argv[])
{
    int i;
    struct options opt;

    progname = argv[0];

    opt.findallmatches = false;
    
    if (argc > 1) 
    {
        for (i = 1; i < argc; i++) 
        { 
            if (strcmp(argv[i], "-a") == 0 || strcmp(argv[i], "/a") == 0) 
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

bool which(LPCTSTR filename, struct options *opt)
{
    LPTSTR path;
    LPTSTR pathext;
    LPTSTR ext;
    bool foundmatch = false;
    bool foundatleastone = false;
    char *cwd;
    char *cwdpart;

    path = getenv(PATH);
    
    if (path != NULL)
    {
        /* We need to add the current working directory to the PATH because
           Windows looks there first. */
        if ((cwd = _getcwd(NULL, 0)) != NULL)
        {
            cwdpart = concat(cwd, ";");
            free(cwd);

            path = concat(cwdpart, path);
            free(cwdpart);

            pathext = getenv(PATHEXT);

            if (pathext != NULL)
            {
                /* The pathext value returned by getenv is not modifiable. So it is
                   necessary to create a copy which is modifiable, using _strdup. 
                   This will have to be freed when we are done with it. The strtok
                   function requires a modifiable variable. */
                pathext = _strdup(pathext);
    
                ext = strtok(pathext, SEMICOLON);
                while (ext != NULL && (opt->findallmatches || !foundmatch))
                {
                    foundmatch = whichext(filename, ext, path);
                    if (foundmatch)
                    {
                        foundatleastone = true;
                    }
                    ext = strtok(NULL, SEMICOLON);
                }
                free(pathext);

                if (! foundatleastone)
                {
                    printf("'%s' was not found in PATH using PATHEXT extensions.\n", filename);
                }
            }
            else 
            {
                fprintf(stderr, "Your PATHEXT environment variable is not set.\n");
            }
            free(path);
        }
        else
        {
            perror("Failed to get current directory");
        }
    }
    else 
    {
        fprintf(stderr, "Your PATH environment variable is not set.\n");
    }
    return foundatleastone;
}

bool whichext(const TCHAR *searchfile, const TCHAR *ext, const TCHAR *path)
{
    TCHAR filefoundinpath[MAX_PATH + 1];
    TCHAR correctname[MAX_PATH + 1];
    TCHAR *namepartonly;
    LPTSTR nameptr;
    DWORD filefoundlen;
    DWORD errorcode;
    WIN32_FIND_DATA filedetails;
    HANDLE findhandle;
    bool foundfile = false;
    DWORD correctnamelen;
    TCHAR drive[_MAX_DRIVE+1];
    TCHAR dir[_MAX_DIR+1];

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
            _splitpath(filefoundinpath, drive, dir, NULL, NULL); 
            printf("%s%s%s\n", drive, dir, filedetails.cFileName);

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
    puts("usage: which [-a] command...");
}

void printerror(const TCHAR *offendingobject, DWORD errorcode)
{
    TCHAR *error;

    error = errortext(errorcode);
    fprintf(stderr, "which: ``%s'': %s\n", offendingobject, error);
    LocalFree(error);
}

/* This function was taken from Microsoft's Knowledge Base Article 149409
   and modified to fix the formatting. */
TCHAR *errortext(DWORD errorcode)
{
    HMODULE messagelocation = NULL; /* NULL means to look for the message in the system module. */
    LPTSTR message = "";
    DWORD messagelength;

    /* If dwLastError is in the network range, load the message source */
    if (NERR_BASE <= errorcode && errorcode <= MAX_NERR) 
    {
        messagelocation = LoadLibraryEx(TEXT("netmsg.dll"), NULL, LOAD_LIBRARY_AS_DATAFILE);
    }

    /* Call FormatMessage() to allow for message text to be acquired
       from the system or the supplied module handle */
    messagelength = FormatMessage(
            FORMAT_MESSAGE_ALLOCATE_BUFFER |
            FORMAT_MESSAGE_IGNORE_INSERTS |
            FORMAT_MESSAGE_FROM_SYSTEM | /* always consider system table */
            ((messagelocation != NULL) ? FORMAT_MESSAGE_FROM_HMODULE : 0),
            messagelocation, /* Module to get message from (NULL == system) */
            errorcode,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
            (LPTSTR) &message,
            0,
            NULL);

    if (messagelength == 0)
    {
        message = concatdword("Error code ", errorcode);
    }

    /* If you loaded a message source, unload it. */
    if (messagelocation != NULL) 
    {
        FreeLibrary(messagelocation);
    }
    return message;
}

/* Works like Java's "+" operator for java.lang.String. */
TCHAR *concat(const TCHAR *s, const TCHAR *t)
{
    TCHAR *result = NULL;
    size_t length;

    if (s == NULL)
    {
        s = "";
    }

    if (t == NULL)
    {
        t = "";
    }

    /* Variables s & t are guaranteed to be non-NULL at this point. */
    length = strlen(s) + strlen(t);

    /* Calculate the exact size of the resulting string when s & t are
       concatenated, plus 1 for the string terminator character. */
    result = (TCHAR *) malloc((length + 1) * sizeof(TCHAR));

    if (result != NULL)
    {
        /* There is no need for special "secure" string functions because
           the size of result has already been correctly calculated so
           a buffer overrun is NOT POSSIBLE. */
        strcpy(result, s);
        strcat(result, t);
        result[length] = '\0';
    }
    else 
    {
        perror(progname);
    }
    return result;
}

TCHAR *concatdword(const TCHAR *s, DWORD n)
{
    TCHAR numbertext[128];

    /* There can be no buffer overrun because an unsigned integer cannot
       exceed the number of characters in the buffer. */
    sprintf(numbertext, "%u", n);
    return concat(s, numbertext);
}

