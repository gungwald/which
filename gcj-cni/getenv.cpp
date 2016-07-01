/**
 * CNI implementation of the POSIX getenv()
 */

#include <cstdlib>
#include <cstdio>
#include <gcj/cni.h>
#include <java/lang/String.h>
#include "Posix.h"

java::lang::String *Posix::getenv(java::lang::String *jvVarName)
{
    int length = JvGetStringUTFLength(jvVarName);
    char name[length + 1];
    char *value = NULL;
    jstring jvValue = NULL;

    jsize size = JvGetStringUTFRegion(jvVarName, 0, length, name);
    name[size] = '\0';
    value = std::getenv(name);                    /* Native POSIX call */
    if (value != NULL) {
        jvValue = JvNewStringUTF(value);
    }
    return jvValue;
}

