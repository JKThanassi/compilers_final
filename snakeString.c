#include "snakeString.h"
#include <bits/stdint-uintn.h>
#include <stdint.h>
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

  // TODO for when I come back:
  // Make a function which will request the correct number of stack space rather than doing this horrible inline asm shit that doesnt work at all

  asm volatile("movq %%r15, $0;"
               "movq %%rbp, $1;"
               "movq %%rsp, $2;"
               : "=r"(heapTop), "=r"(currFrame), "=r"(stackTop));

  uint64_t *newHeapTop =
      try_gc(heapTop, numWords * wordSize, currFrame, stackTop);

  asm volatile("movq $0, %%r15;" : : "r"(newHeapTop));
  *newHeapTop = strLen;
  memset(newHeapTop + 1, '\0', strLen);

  int numBytes = numWords * wordSize;
  asm volatile("addq $0, %%r15;" : : "m"(numBytes));
  uint64_t taggedStr = ((uint64_t) newHeapTop) + SNAKE_STRING_TAG;
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
