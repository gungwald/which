#include <jni.h>
#include <stdlib.h>     /* getenv */
#include "PlainPosix.h" /* Java_PlainPosix_getenv prototype */

JNIEXPORT jstring JNICALL Java_PlainPosix_getenv(JNIEnv *env, jclass cls, jstring jname)
{
    const char *name;
    char *value = NULL;
    jstring jvalue = NULL;

    name = (*env)->GetStringUTFChars(env, jname, NULL);
    if (name != NULL) {
        value = getenv(name);
        if (value != NULL) {
            jvalue = (*env)->NewStringUTF(env, value);
        }
        (*env)->ReleaseStringUTFChars(env, jname, name);
    }
    return jvalue;
}

