#include "snakeString.h"

char *snakeStringToCString(uint64_t val)
{
    if (isSnakeString(val))
    {
        uint64_t *snakeStringPtr = (uint64_t *)(val - SNAKE_STRING_TAG);
        uint64_t len = snakeStringPtr[0];
        // need len + 1 to add null terminal
        char *convertedStr = malloc(sizeof(char) * (len + 1));
        // offset by 1 so we copy only characters instead of the length
        for (size_t i = 0; i < len; i++)
        {
            char temp = snakeStringPtr[1 + i];
            convertedStr[i] = temp;
        }
        // set last char to be null terminal
        convertedStr[len] = '\0';
        return convertedStr;
    }
    else
    {
        error(ERR_VAL_NOT_STRING, val);
    }
}

bool isSnakeString(uint64_t val)
{
    return (val & SNAKE_STRING_MASK) == SNAKE_STRING_TAG;
}