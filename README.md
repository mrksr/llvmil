# LLVM-IL
Scala-Library used to emit a subset of the textual LLVM-IR Code. Besides the
direct commands, it contains some specific OOP features, like the creation of
simple V-Tables paired with field access and virtual resolve. It works together
with a simple runtime written in C (for which the function declaration exist in
the library).

## Compilation

Just compile with sbt.

    sbt compile

## Usage
The code is organized in classes which have functions and fields, with the
addition of static global functions. To create a simple List-Class, we first
initialize the program and then add a dummy class `Object` with an `id`-Field,
to showcase some inheritance.

```scala
val prog = new Program()

val parent = prog.addClass("Object", None)
parent.addField("id", TInt)
```

For this, we then add getters and setters with appropriate LLVM code.

```scala
locally {
  implicit val mtd = parent.addMethod("setID", List((TInt, "to")), TVoid, None)

  mtd append (
    AccessField(This, "id", TInt) +>
    ( Store(Local(TInt, "to"), _ ) ) ::
    RetVoid
  )
}

locally {
  implicit val mtd = parent.addMethod("getID", Nil, TInt, None)

  mtd append (
    AccessField(This, "id", TInt) ++
    ( Load(_) ) +>
    ( Ret(_) )
  )
}
```

Our list class `MyIntList` inherits from this `Object` class. We override the
`getID`-Method and add some simple getters and setters.

```scala
val cls = prog.addClass("MyIntList", Some("Object"))
cls.addField("Value", TInt)
cls.addField("Prev", TReference("MyIntList"))
cls.addField("Next", TReference("MyIntList"))

locally {
  implicit val mtd = cls.addMethod("getID", Nil, TInt, Some(("getID", Nil, TInt)))

  mtd append (
    Ret(Const(42))
  )
}

locally {
  implicit val mtd = cls.addMethod("setVal", List((TInt, "to")), TVoid, None)

  mtd append (
    AccessField(This, "Value", TInt) +>
    ( Store(Local(TInt, "to"), _ ) ) ::
    RetVoid
  )
}

locally {
  implicit val mtd = cls.addMethod("getVal", List(), TInt, None)

  mtd append (
    AccessField(This, "Value", TInt) ++
    ( Load(_) ) +>
    ( Ret(_) )
  )
}
```

To make things interesting, we lastly add an accessor for the value of the next
element in the list using a virtual resolve.

```scala
locally {
  implicit val mtd = cls.addMethod("nextVal", List(), TInt, None)

  mtd append (
    AccessField(This, "Next", TReference("MyIntList")) ++
    ( Load(_) ) ++
    ( VirtualResolve(_, "getVal", Nil, TInt) ) ++
    ( Call(_, List(This)) ) +>
    ( Ret(_) )
  )
}
```

Lastly we create some simple main method and export the llvm code.

```scala
locally {
  import Runtime._
  implicit val main = prog.addMain()

  main append (
    Call(string_equals, List(
      Bitcast(TString, Const("Hello World")),
      Bitcast(TString, Const("Wello Horld"))
    )) +>
    (id => CallVoid(println_bool, List(id) ) ) ::
    Ret(Const(1))
  )
}

prog.writeToFile("myList.ll")
```

## LLVM-IR code
The library then takes care of most of the types and temporary variable names,
resulting in LLVM-IL code which looks like this.

```llvm
define i32 @method.MyIntList.nextVal(i8* %this) {
  %t1 = bitcast i8* %this to %struct.class.MyIntList*
  %1 = getelementptr %struct.class.MyIntList* %t1, i32 0, i32 4
  %2 = load %struct.class.MyIntList** %1
  %t2 = getelementptr %struct.class.MyIntList* %2, i32 0, i32 0
  %t3 = load [5 x void (...)*]** %t2
  %t4 = getelementptr [5 x void (...)*]* %t3, i32 0, i32 3
  %t5 = load void (...)** %t4
  %3 = bitcast void (...)* %t5 to i32 (i8*)*
  %4 = call i32 %3(i8* %this)
  ret i32 %4
}
```

This uses the V-Tables defined automatically.

```llvm
%struct.class.Object = type {
  [2 x void (...)*]*,
  i32
}
%struct.class.MyIntList = type {
  [5 x void (...)*]*,
  i32,
  i32,
  %struct.class.MyIntList*,
  %struct.class.MyIntList*
}

@vtable.Object = constant [2 x void (...)*] [
  void (...)* bitcast(void (i8*, i32)* @method.Object.setID to void (...)*),
  void (...)* bitcast(i32 (i8*)* @method.Object.getID to void (...)*)
]
@vtable.MyIntList = constant [5 x void (...)*] [
  void (...)* bitcast(void (i8*, i32)* @method.Object.setID to void (...)*),
  void (...)* bitcast(i32 (i8*)* @method.MyIntList.getID to void (...)*),
  void (...)* bitcast(void (i8*, i32)* @method.MyIntList.setVal to void (...)*),
  void (...)* bitcast(i32 (i8*)* @method.MyIntList.getVal to void (...)*),
  void (...)* bitcast(i32 (i8*)* @method.MyIntList.nextVal to void (...)*)
]
```
