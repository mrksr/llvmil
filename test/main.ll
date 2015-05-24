

;@.str = private unnamed_addr constant [6 x i8] c"w00t?\00", align 1
@hello = constant { i32, [12 x i8] } { i32 11, [12 x i8] c"Hello World\00" }
@sz$0 = constant [ 17 x i8 ] c"A runtime String\00"
@sz$1 = constant [ 25 x i8 ] c" and more runtime String\00"

define i32 @main() {
entry:
    call void @println_string(%struct.String* bitcast ({ i32, [12 x i8] }* @hello to %struct.String*))
    call void @println_bool(i1 0)
    %str.0 = call %struct.String* @string_create(i8* bitcast ([17 x i8]* @sz$0 to i8*))
    %str.1 = call %struct.String* @string_create(i8* bitcast ([25 x i8]* @sz$1 to i8*))
    %str = call %struct.String* @string_concat(%struct.String* %str.0, %struct.String* %str.1)
    call void @println_string(%struct.String* %str)
    call void @println_int(i32 75)
    ret i32 0
}

