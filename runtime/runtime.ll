
%String   = type { i32, [0 x i8] }
%IntArray = type { i32, [0 x i32] }

declare i8* @kool_alloc(i32)

declare %String* @string_alloc(i32)
declare %String* @string_create(i8* nocapture)
declare %String* @string_concat(%String* nocapture, %String* nocapture)
declare i1       @string_equals(%String* nocapture, %String* nocapture)

declare %IntArray @array_alloc(i32)

declare void @println_int(i32)
declare void @println_string(%String* nocapture)
declare void @println_bool(i1)

