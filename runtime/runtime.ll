
%struct.String   = type { i32, [0 x i8] }
%struct.IntArray = type { i32, [0 x i32] }

declare i8* @kool_alloc(i32)

declare %struct.String* @string_alloc(i32)
declare %struct.String* @string_create(i8* nocapture)
declare %struct.String* @string_concat(%struct.String* nocapture, %struct.String* nocapture)
declare i1       @string_equals(%struct.String* nocapture, %struct.String* nocapture)

declare %struct.IntArray @array_alloc(i32)

declare void @println_int(i32)
declare void @println_string(%struct.String* nocapture)
declare void @println_bool(i1)

