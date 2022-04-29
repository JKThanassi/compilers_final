#ifndef SNAKE_STRING_H
#define SNAKE_STRING_H

#include <bits/stdint-uintn.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

static const uint64_t SNAKE_STRING_TAG = 0x0000000000000007;
static const uint64_t SNAKE_STRING_MASK = 0x000000000000000F;
static const uint64_t ERR_VAL_NOT_STRING = 18;
static const uint64_t ERR_SUBSTRING_BAD_ARGS = 19;


// fwd decl of try_gc
uint64_t *try_gc(uint64_t *alloc_ptr, uint64_t bytes_needed, uint64_t *cur_frame, uint64_t *cur_stack_top);

/**
 * takes the number of words the snake string will occupy and the length of the string
 * returns a tagged snake string and updates the heap pointer
 */
uint64_t newSnakeStringOfLen(uint64_t strLen);

extern uint64_t create_empty_snake_str(uint64_t numBytes, uint64_t strLen) asm("create_empty_snake_str");

// forward decl of error
void error(uint64_t code, uint64_t val);
bool isSnakeString(uint64_t val);
/**
 * Converts snake strings to a null terminated char array.
 */
char *snakeStringToCString(uint64_t val);
extern uint64_t snakeStringCmp(uint64_t s1, uint64_t s2) asm("snakeStringCmp");
extern uint64_t snakeStringConcat(uint64_t s1, uint64_t s2) asm("snakeStringConcat");
/**
 * start must be > 0, end must be > 0 and end > start
 * end must be less than the length of the string
 * if these conditions are not met an error will be thrown
 * Another note, the indicies here are 0 indexed 
 */
uint64_t snakeStringSubstring(uint64_t s1, uint64_t start, uint64_t end) asm("snakeStringSubstring");
uint64_t snakeStringIdxOf(uint64_t str, uint64_t toSeek);
uint64_t snakeStringSplit(uint64_t str, uint64_t idx);
uint64_t snakeStringReplace(uint64_t str, uint64_t toReplace, uint64_t replaceWith);
uint64_t snakeStringContains(uint64_t str, uint64_t contains);
uint64_t snakeStringToUpper(uint64_t str);
uint64_t snakeStringToLower(uint64_t str);
uint64_t snakeStringTrim(uint64_t str);

#endif
