/*
 * errors.c
 *
 *  Created on: Aug 8, 2016
 *      Author: Bill.Chatfield
 */

#include <stdio.h>
#include <lmerr.h>
#include <windows.h>

#include "errors.h"

void printerror(const wchar_t *offendingobject, DWORD errorcode)
{
    wchar_t *error;

    error = errortext(errorcode);
    fwprintf(stderr, L"which: ``%s'': %s\n", offendingobject, error);
    LocalFree(error);
}

/* This function was taken from Microsoft's Knowledge Base Article 149409
   and modified to fix the formatting. */
wchar_t *errortext(DWORD errorcode)
{
    HMODULE messagelocation = NULL; /* NULL means to look for the message in the system module. */
    wchar_t *message = L"";
    DWORD messagelength;

    /* If dwLastError is in the network range, load the message source */
    if (NERR_BASE <= errorcode && errorcode <= MAX_NERR)
    {
        messagelocation = LoadLibraryEx(L"netmsg.dll", NULL, LOAD_LIBRARY_AS_DATAFILE);
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
            (wchar_t *) &message,
            0,
            NULL);

    if (messagelength == 0)
    {
        message = concatUInt32(L"Error code ", errorcode);
    }

    /* If you loaded a message source, unload it. */
    if (messagelocation != NULL)
    {
        FreeLibrary(messagelocation);
    }
    return message;
}

