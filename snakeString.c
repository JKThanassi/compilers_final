#include "snakeString.h"
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
  strncat(char *, const char *, unsigned long);
}
