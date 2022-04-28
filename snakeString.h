#ifndef SNAKE_STRING_H
#define SNAKE_STRING_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

static const uint64_t SNAKE_STRING_TAG = 0x0000000000000007;
static const uint64_t SNAKE_STRING_MASK = 0x0000000000000007;
static const uint64_t ERR_VAL_NOT_STRING = 18;

// forward decl of error
void error(uint64_t code, uint64_t val);
bool isSnakeString(uint64_t val);
char *snakeStringToCString(uint64_t val);
uint64_t snakeStringCmp(uint64_t s1, uint64_t s2);
uint64_t snakeStringConcat(uint64_t s1, uint64_t s2);
uint64_t snakeStringSubstring(uint64_t s1, uint64_t start, uint64_t end);
uint64_t snakeStringIdxOf(uint64_t str, uint64_t toSeek);
uint64_t snakeStringSplit(uint64_t str, uint64_t idx);
uint64_t snakeStringReplace(uint64_t str, uint64_t toReplace, uint64_t replaceWith);
uint64_t snakeStringContains(uint64_t str, uint64_t contains);
uint64_t snakeStringToUpper(uint64_t str);
uint64_t snakeStringToLower(uint64_t str);
uint64_t snakeStringTrim(uint64_t str);

#endif