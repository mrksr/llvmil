
#ifdef __cplusplus
extern "C" {
#endif

#include <gc.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>


/************************************************************
 * Basic Types
 ************************************************************/

typedef bool Bool;
typedef int32_t Int;

typedef struct {
    Int size;
    char s[0];
} TString, * String;

typedef struct {
    Int size;
    Int a[0];
} TIntArray, *IntArray;


/************************************************************
 * memory management
 ************************************************************/

void* kool_alloc(Int size) { return GC_malloc(size); }


/************************************************************
 * String functions
 ************************************************************/

// @String = type { i32 size, [0 x i8] s }
// @StrLit1 = { i32 5, [6 x i8] "Hello\0" }
// call %fn( @String bitcast @StrLit1 to @String, args... )

String string_alloc(Int length) {
    String r = (String)kool_alloc(sizeof(TString) + (length+1)*(sizeof(char)));
    r->size = length;
    return r;
}

String string_create(const char* sz) {
    Int length = strlen(sz);
    String r = string_alloc(length);
    memcpy(r->s, sz, (length+1)*sizeof(char));
    return r;
}

String string_concat(String s1, String s2) {
    String r = string_alloc(s1->size + s2->size);
    memcpy( r->s,                s1->s, (s1->size)   *sizeof(char));
    memcpy( &(r->s[s1->size]),   s2->s, (s2->size+1) *sizeof(char));
    return r;
}

bool string_equals(String s1, String s2) {
    return strcmp(s1->s, s2->s) == 0;
}


/************************************************************
 * IntArray functions
 ************************************************************/

// @IntArray = type { i32 size, [0 x i32] a }

IntArray array_alloc(Int length) {
    IntArray r = (IntArray)kool_alloc(sizeof(TIntArray) + length*(sizeof(Int)));
    r->size = length;
    return r;
}


/************************************************************
 * println
 ************************************************************/

void println_int(Int value) {
    printf("%d\n", value);
}

void println_string(String value) {
    printf("%s\n", value->s);
}

void println_bool(Bool value) {
    if(value)
        printf("True\n");
    else
        printf("False\n");
}


/************************************************************
 * end
 ************************************************************/

#ifdef __cplusplus
} // extern "C"
#endif


