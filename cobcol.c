/*
    cobcol.c
    A tool to generate helper columns for COBOL records.
    If you want type records for:
        01 NAME    PIC X(10).
        01 ID      PIC 9(4).
        01 PRICE   PIC 99V99.

    you type:
        cobcol "X(10);N(4);N(4)"

    output:
        XXXXXXXXXX99997777

    This helps in typing the records accordingly:
        XXXXXXXXXX99997777
        SIMON SULS12350850
        TESTER TES45671250

    (c) 2024 by ssulser        
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

// MAX_NUMBER = maximum digits + '\0'
#define MAX_NUMBER 4

// parse command string
int parse(const char *commands) {
    const char *ptr = commands;
    char command;
    int number;
    int last_alpha_X = 0;
    int last_number_9 = 0;

    while (*ptr != '\0') {
        // skip any leading spaces or semicolons
        while (*ptr == ' ' || *ptr == ';') {
            ptr++;
        }
        
        // check if the current character is a valid command (A, B, C)
        if (isalpha(*ptr)) {
            command = *ptr;
            ptr++;

            // ensure the next character is an opening parenthesis
            if (*ptr == '(') {
                ptr++;
                char number_string[MAX_NUMBER];
                int index = 0;

                // extract the number inside the parenthesis
                while (isdigit(*ptr) && index < MAX_NUMBER) {
                    number_string[index++] = *ptr++;
                }
                number_string[index] = '\0';

                number = atoi(number_string);

                // ensure the next character is a closing parenthesis
                if (*ptr == ')') {
                    ptr++;

                    // was command X for alphanumeric PIC
                    // or N for numeric PIC
                    switch (command)
                    {
                    case 'X':
                        // ensure that two neighbouring alphanumeric definitions
                        // don't use the same character
                        for (int i=0; i<number; i++) {
                            putchar(last_alpha_X ? 'A' : 'X');
                        }
                        last_alpha_X = ~last_alpha_X;
                        last_number_9 = 0;
                        break;
                    case 'N':
                        // ensure that two neighbouring numeric definitions
                        // don't use the same character
                        for (int i=0; i<number; i++) {
                            putchar(last_number_9 ? '7' : '9');
                        }
                        last_alpha_X = 0;
                        last_number_9 = ~last_number_9;
                    default:
                        break;
                    }
                } else {
                    fprintf(stderr, "Error: Expected closing parenthesis\n");
                    return 1;
                }
            } else {
                fprintf(stderr, "Error: Expected opening parenthesis\n");
                return 1;
            }
        } else if (*ptr != '\0') {
            fprintf(stderr, "Error: Invalid command character\n");
            return 1;
        }
    }
    printf("\n");
    return 0;
}

int main(int argc, char const *argv[])
{
    if (argc == 1 || parse(argv[1])) {
        fprintf(stderr,
            "USAGE: %s commandline\n"                                \
            "A tool to generate helper columns for COBOL records.\n" \
            "(c) 2024 by ssulser\n\n"                                \
            "commandline:\tenclose in '\n"                           \
            "\t\tX(n) - alphanumeric\n"                              \
            "\t\tN(n) - numeric\n"                                   \
            "\nIf you want type records for:\n"                      \
            "\t01 NAME    PIC X(10).\n"                              \
            "\t01 ID      PIC 9(4).\n"                               \
            "\t01 PRICE   PIC 99V99.\n\n"                            \
            "you type:\n"                                            \
            "\tcobcol\t'X(10);N(4);N(4)' or\n"                       \
            "\t\t'X(10)N(4)N(4)' or\n"                               \
            "\t\t'X(10) N(4) N(4)'\n\n"                              \
            "output:\n"                                              \
            "\tXXXXXXXXXX99997777\n\n"                               \
            "This helps in typing the records accordingly:\n"        \
            "\tXXXXXXXXXX99997777\n"                                 \
            "\tSIMON SULS12350850\n"                                 \
            "\tTESTER TES45671250\n\n"
        , argv[0]);
    }
    return 0;
}