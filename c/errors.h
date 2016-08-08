/*
 * errors.h
 *
 *  Created on: Aug 8, 2016
 *      Author: Bill.Chatfield
 */

#ifndef ERRORS_H_
#define ERRORS_H_


wchar_t *errortext(unsigned long errorcode);
void printerror(const wchar_t *offendingobject, unsigned long errorcode);


#endif /* ERRORS_H_ */
