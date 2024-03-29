#ifndef SNAKE_STRING_H
#define SNAKE_STRING_H

#include <bits/stdint-uintn.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const uint64_t SNAKE_STRING_TAG = 0x0000000000000007;
static const uint64_t SNAKE_STRING_MASK = 0x000000000000000F;
static const uint64_t ERR_VAL_NOT_STRING = 18;
static const uint64_t ERR_SUBSTRING_BAD_ARGS = 19;

// fwd decl of try_gc
uint64_t *try_gc(uint64_t *alloc_ptr, uint64_t bytes_needed,
                 uint64_t *cur_frame, uint64_t *cur_stack_top);

/**
 * takes the number of words the snake string will occupy and the length of the
 * string returns a tagged snake string and updates the heap pointer
 */
uint64_t newSnakeStringOfLen(uint64_t strLen);

extern uint64_t create_empty_snake_str(uint64_t numBytes, uint64_t strLen) asm(
    "create_empty_snake_str");

// forward decl of error
void error(uint64_t code, uint64_t val);
bool isSnakeString(uint64_t val);
/**
 * Converts snake strings to a null terminated char array.
 */
char *snakeStringToCString(uint64_t val);
/**
 * Lexographically compares two strings.
 *
 * Args:
 * s1: A snakeString
 * s2: A snakeString which will be compared to s1
 *
 * returns: a positive snakeNum if s1 is greater than s2, a negative snakeNum if
 * s2 is greater than s1, and 0 if they are equal.
 */
extern uint64_t snakeStringCmp(uint64_t s1, uint64_t s2) asm("snakeStringCmp");
/**
 * Args:
 * s1: A snakeString to be contatenated to
 * s2: A snakeString which will be contatenated to s1
 *
 * returns: A new snakeString containing the contents of s1 appended to s2
 */
extern uint64_t snakeStringConcat(uint64_t s1,
                                  uint64_t s2) asm("snakeStringConcat");
/**
 * Args:
 * s1: the snakeString to take the substring of
 * start: the snake num representing the start of the substring. (0 indexed,
 * inclusive) end: the snake num representing the end of the substring
 * (exclusive)
 *
 * start must be > 0, end must be > 0 and end > start
 * end must be less than the length of the string
 * if these conditions are not met an error will be thrown
 * Another note, the indicies here are 0 indexed
 */
uint64_t snakeStringSubstring(uint64_t s1, uint64_t start,
                              uint64_t end) asm("snakeStringSubstring");
uint64_t snakeStringIdxOf(uint64_t str, uint64_t toSeek);
uint64_t snakeStringReplace(uint64_t str, uint64_t toReplace,
                            uint64_t replaceWith);
uint64_t snakeStringContains(uint64_t str, uint64_t contains);
/**
 * Converts string to uppercase in place
 *
 * Args:
 * str: the snakeString to convert
 *
 * returns:
 * The pointer to the snakeString passed in with the contents changed to
 * uppercase
 */
uint64_t snakeStringToUpper(uint64_t str);
/**
 * Converts string to lowercase in place
 *
 * Args:
 * str: the snakeString to convert
 *
 * returns:
 * The pointer to the snakeString passed in with the contents changed to
 * lowercase
 */
uint64_t snakeStringToLower(uint64_t str);
/**
 * Returns a new string with the leading and trailing whitespace removed
 *
 * Args:
 * str: the snakeString to trim
 *
 * returns:
 * The pointer to a new snakeString with the whitespace removed
 */
uint64_t snakeStringTrim(uint64_t str);

#endif
