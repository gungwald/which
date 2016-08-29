/*
 * errors.h
 *
 *  Created on: Aug 8, 2016
 *      Author: Bill.Chatfield
 */

#ifndef ERRORS_H
#define ERRORS_H

#include <wchar.h>

wchar_t *getErrorMessage(unsigned long errorCode);
void printErrorMessage(const wchar_t *offendingObject, unsigned long errorCode);


#endif /* ERRORS_H */
