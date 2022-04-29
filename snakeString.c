#include "snakeString.h"
#include <bits/stdint-uintn.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>

typedef struct {
  uint64_t len;
  char *contents;
} snakeStringComponents;

snakeStringComponents *ptrToComponents(uint64_t val) {
  if (!isSnakeString(val)) {
    error(ERR_VAL_NOT_STRING, val);
  }
  uint64_t *snakeStringPtr = (uint64_t *)(val - SNAKE_STRING_TAG);
  char *snakeStringStart =
      (char *)(snakeStringPtr +
               1); // Offset by 1 word to get the characters stored in the tuple
  uint64_t len = snakeStringPtr[0];
  snakeStringComponents *toRet = malloc(sizeof(snakeStringComponents));
  toRet->contents = snakeStringStart;
  toRet->len = len;
  return toRet;
}

uint64_t newSnakeStringOfLen(uint64_t strLen) {
  uint64_t *heapTop, *currFrame, *stackTop;
  const int wordSize = 8;

  uint64_t numWords = (strLen / wordSize) + 1;
  numWords = ((strLen % wordSize) == 0) ? numWords : numWords + 1;
  numWords = (numWords % 2) == 0 ? numWords : numWords + 1;

  int numBytes = numWords * wordSize;
  uint64_t untaggedStr = create_empty_snake_str(numBytes, strLen);
  uint64_t taggedStr = untaggedStr + SNAKE_STRING_TAG;
  return taggedStr;
}

char *snakeStringToCString(uint64_t val) {
  snakeStringComponents *components = ptrToComponents(val);
  // need len + 1 to add null terminal
  char *convertedStr = malloc(sizeof(char) * (components->len + 1));
  // offset by 1 so we copy only characters instead of the length
  strncpy(convertedStr, components->contents, components->len);
  // set last char to be null terminal
  convertedStr[components->len] = '\0';
  free(components);
  return convertedStr;
}

bool isSnakeString(uint64_t val) {
  return (val & SNAKE_STRING_MASK) == SNAKE_STRING_TAG;
}

uint64_t snakeStringCmp(uint64_t s1, uint64_t s2) {
  snakeStringComponents *s1c = ptrToComponents(s1);
  snakeStringComponents *s2c = ptrToComponents(s2);
  if (s1c->len > s2c->len) {
    free(s1c);
    free(s2c);
    return 2;
  }
  if (s1c->len < s2c->len) {
    free(s1c);
    free(s2c);
    return -2;
  }
  int64_t val = strncmp(s1c->contents, s2c->contents, s1c->len);
  free(s1c);
  free(s2c);
  return val << 1;
}

uint64_t snakeStringConcat(uint64_t s1, uint64_t s2) {
  snakeStringComponents *s1c = ptrToComponents(s1);
  snakeStringComponents *s2c = ptrToComponents(s2);
  uint64_t toRet = newSnakeStringOfLen(s1c->len + s2c->len);
  snakeStringComponents *sDestC = ptrToComponents(toRet);
  strncpy(sDestC->contents, s1c->contents, s1c->len);
  strncpy(sDestC->contents + s1c->len, s2c->contents, s2c->len);
  return toRet;
}

uint64_t snakeStringSubstring(uint64_t s1, uint64_t start, uint64_t end) {
  snakeStringComponents *s1c = ptrToComponents(s1);
  uint64_t shiftedStart = ((int64_t) start) >> 1;
  uint64_t shiftedEnd = ((int64_t) end) >> 1;
  if (shiftedStart < 0 || shiftedEnd < 0 || shiftedEnd < shiftedStart || shiftedEnd > s1c->len) {
    error(ERR_SUBSTRING_BAD_ARGS, start); 
  }
  uint64_t len = shiftedEnd - shiftedStart;
  uint64_t dest = newSnakeStringOfLen(len);
  snakeStringComponents *destC = ptrToComponents(dest);
  strncpy(destC->contents, s1c->contents + shiftedStart, len);
  return dest; 
}

uint64_t snakeStringToUpper(uint64_t str) {
  snakeStringComponents *strC = ptrToComponents(str);
  for (int i = 0; i < strC->len; i++) {
    strC->contents[i] = toupper(strC->contents[i]);
  }
  return str;
}

uint64_t snakeStringToLower(uint64_t str) {
  snakeStringComponents *strC = ptrToComponents(str);
  for (int i = 0; i < strC->len; i++) {
    strC->contents[i] = tolower(strC->contents[i]);
  }
  return str;
}

uint64_t snakeStringTrim(uint64_t str) {
  snakeStringComponents *strC = ptrToComponents(str);
  if (strC->len == 0) return str;

  int startIdx = 0;
  while (isspace(strC->contents[startIdx])) startIdx++;

  int endIdx = strC->len - 1;
  while (endIdx > startIdx && isspace(strC->contents[endIdx])) endIdx--;

  int len = (endIdx - startIdx) + 1;

  uint64_t toRet = newSnakeStringOfLen(len);
  snakeStringComponents *sDestC = ptrToComponents(toRet);
  strncpy(sDestC->contents, strC->contents + startIdx, len);
  return toRet;
}
