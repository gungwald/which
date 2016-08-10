/*
 * errors.h
 *
 *  Created on: Aug 8, 2016
 *      Author: Bill.Chatfield
 */

#ifndef ERRORS_H_
#define ERRORS_H_


wchar_t *getMessage(unsigned long errorCode);
void printError(const wchar_t *offendingObject, unsigned long errorCode);


#endif /* ERRORS_H_ */
