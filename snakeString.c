#include "snakeString.h"

#include <ctype.h>
#include <stdint.h>
#include <string.h>

snakeStringComponents *ptrToComponents(uint64_t val) {
  if (!isSnakeString(val)) {
    error(ERR_VAL_NOT_STRING, val);
  }
  uint64_t *snakeStringPtr = (uint64_t *)(val - SNAKE_STRING_TAG);
  char *snakeStringStart =
      (char *)(snakeStringPtr + 1);  // Offset by 1 word to get the characters
                                     // stored in the tuple
  uint64_t len = snakeStringPtr[0];
  snakeStringComponents *toRet = malloc(sizeof(snakeStringComponents));
  toRet->contents = snakeStringStart;
  toRet->len = len;
  return toRet;
}

int numWordsForStringOfLen(uint64_t strLen) {
  int wordSize = 8;
  uint64_t numWords = (strLen / wordSize) + 1;
  numWords = ((strLen % wordSize) == 0) ? numWords : numWords + 1;
  numWords = (numWords % 2) == 0 ? numWords : numWords + 1;

  return numWords;
}

uint64_t newSnakeStringOfLen(uint64_t strLen) {
  const int wordSize = 8;

  uint64_t numWords = numWordsForStringOfLen(strLen);
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
  free(s1c);
  free(s2c);
  return toRet;
}

uint64_t snakeStringSubstring(uint64_t s1, uint64_t start, uint64_t end) {
  snakeStringComponents *s1c = ptrToComponents(s1);
  uint64_t shiftedStart = ((int64_t)start) >> 1;
  uint64_t shiftedEnd = ((int64_t)end) >> 1;
  if (shiftedStart < 0 || shiftedEnd < 0 || shiftedEnd < shiftedStart ||
      shiftedEnd > s1c->len) {
    free(s1c);
    error(ERR_SUBSTRING_BAD_ARGS, start);
  }
  uint64_t len = shiftedEnd - shiftedStart;
  uint64_t dest = newSnakeStringOfLen(len);
  snakeStringComponents *destC = ptrToComponents(dest);
  strncpy(destC->contents, s1c->contents + shiftedStart, len);
  free(destC);
  free(s1c);
  return dest;
}

uint64_t snakeStringToUpper(uint64_t str) {
  snakeStringComponents *strC = ptrToComponents(str);
  for (int i = 0; i < strC->len; i++) {
    strC->contents[i] = toupper(strC->contents[i]);
  }
  free(strC);
  return str;
}

uint64_t snakeStringToLower(uint64_t str) {
  snakeStringComponents *strC = ptrToComponents(str);
  for (int i = 0; i < strC->len; i++) {
    strC->contents[i] = tolower(strC->contents[i]);
  }
  free(strC);
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
  free(strC);
  free(sDestC);
  return toRet;
}

uint64_t snakeStringIdxOf(uint64_t str, uint64_t substr) {
  snakeStringComponents *strC = ptrToComponents(str);
  snakeStringComponents *substrC = ptrToComponents(substr);

  if (substrC->len == 0) return 0;

  for (int x = 0; x <= strC->len - substrC->len; x++) {
    // Converting these to snake nums since snakeString functions assume values
    // are snakeNumbers
    uint64_t snakeX = ((int64_t)x) << 1;
    uint64_t snakeLen = ((int64_t)substrC->len) << 1;
    if (snakeStringCmp(snakeStringSubstring(str, snakeX, snakeX + snakeLen),
                       substr) == 0) {
      free(strC);
      free(substrC);
      return ((int64_t)x) << 1;
    }
  }
  free(strC);
  free(substrC);
  error(ERR_SUBSTR_NOT_FOUND, str);
}

uint64_t snakeStringContains(uint64_t str, uint64_t substr) {
  snakeStringComponents *strC = ptrToComponents(str);
  snakeStringComponents *substrC = ptrToComponents(substr);

  if (substrC->len == 0) return SNAKE_TRUE;

  for (int x = 0; x <= strC->len - substrC->len; x++) {
    // Converting these to snake nums since snakeString functions assume values
    // are snakeNumbers
    uint64_t snakeX = ((int64_t)x) << 1;
    uint64_t snakeLen = ((int64_t)substrC->len) << 1;
    if (snakeStringCmp(snakeStringSubstring(str, snakeX, snakeX + snakeLen),
                       substr) == 0) {
      free(strC);
      free(substrC);
      return SNAKE_TRUE;
      ;
    }
  }
  free(strC);
  free(substrC);
  return SNAKE_FALSE;
}

uint64_t snakeStringReplace(uint64_t str, uint64_t toReplace,
                            uint64_t replaceWith) {
  snakeStringComponents *strC = ptrToComponents(str);
  snakeStringComponents *toReplaceC = ptrToComponents(toReplace);
  snakeStringComponents *replaceWithC = ptrToComponents(replaceWith);

  uint64_t idxOf = snakeStringIdxOf(str, toReplace);
  uint64_t convertedLengthToReplace = ((int64_t)toReplaceC->len) << 1;
  int convertedIdxOf = ((int64_t)idxOf) >> 1;
  uint64_t startStr, startAndReplaceWith;
  if (convertedIdxOf == 0) {
    startAndReplaceWith = replaceWith;
  } else {
    startStr = snakeStringSubstring(str, 0, idxOf);
    startAndReplaceWith = snakeStringConcat(startStr, replaceWith);
  }
  if ((convertedIdxOf + toReplaceC->len) == strC->len) {
    free(strC);
    free(toReplaceC);
    free(replaceWithC);
    return startAndReplaceWith;
  } else {
    uint64_t convertedStrLen = ((int64_t)strC->len) << 1;
    uint64_t endStr = snakeStringSubstring(
        str, idxOf + convertedLengthToReplace, convertedStrLen);
    free(strC);
    free(toReplaceC);
    free(replaceWithC);
    return snakeStringConcat(startAndReplaceWith, endStr);
  }
}
