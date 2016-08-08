/*
 * stringconcat.h
 *
 *  Created on: Aug 8, 2016
 *      Author: Bill.Chatfield
 */

#ifndef STRINGCONCAT_H_
#define STRINGCONCAT_H_

#define concat(s,t) concat2(s,t)

extern wchar_t *concat2(const wchar_t *s, const wchar_t *t);
extern wchar_t *concat3(const wchar_t *s, const wchar_t *t, const wchar_t *u);
extern wchar_t *concatUInt32(const wchar_t *s, uint32_t n);
extern wchar_t *concatv(const wchar_t *s, const wchar_t *t, ...);


#endif /* STRINGCONCAT_H_ */
