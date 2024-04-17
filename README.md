RockVM
======

A basic virtual machine for dynamically-typed languages. To build, just do `cargo build`.

```console
$ rockvm file.bin    # execute the file by calling the function "main" in the class named "$self"
$ rockvm file.bin -d # disassemble the file
```

# Specs

## The binary format

This is a little-endian VM.

### Overview

u32: magic number\
u8: version

u32: count\
then 'count' String

u32: number of classes\
then 'count' Class

#### Magic number

'ROCK' in ASCII (or 0x4B434F52 in LE)

#### Version

Must be 1.

#### String

u16: length\
then 'length' bytes

#### Class

String: name\
String: super type

u8: count\
then 'count' fields

u8: count\
then 'count' functions

#### Field

String: name\
bytecode: default value

#### Function

String: name\
u8: arity (number of parameters)\
u8: number of local variables\
bytecode: body of the function

#### Bytecode

u8/u16: u8 for a field's default value, u16 for a function's body\
then that many bytes

## Opcodes

### OP_RETURN (1)

Pops the current call frame.

### OP_CONSTANT (2)

Pushes a constant on the stack.

The next byte tells the type of constant, some have following bytes for the actual data:\
`null`: `1`.\
`bool`: `2` then `0` for `false` or `1` (or any non `0`) for `true`.\
`integer`: `3` then 4 bytes containing the integer in little-endian.\
`float`: `4` then 4 bytes containing the float in little-endian.\
`string`: `5` then 2 bytes containing an index in the `strings` array.

### OP_NEGATE (3)

Negates the top value on the stack (int or float only).

### OP_ADD (4)

Pops 2 values from the stack and pushes their sum.

If one of the value is a string, it converts the other to a string and concatenates them.

### OP_SUB (5)

Pops 2 values from the stack and pushes their difference.

### OP_MUL (6)

Pops 2 values from the stack and pushes their product.

### OP_DIV (7)

Pops 2 values from the stack and pushes their fraction.

### OP_TRUE (8)

Pushes `true`.

### OP_FALSE (9)

Pushes `false`.

### OP_NULL (10)

Pushes `null`.

### OP_NOT (11)

Negates the top of the stack if it's a bool.

### OP_EQUAL (12)

Pops 2 values from the stack and pushes `true` if they are equals, `false` otherwise.

### OP_LOWER_THAN (13)

Pops 2 values from the stack and pushes `true` if they are equals, `false` otherwise.

### OP_GREATER_THAN (14)

Pops 2 values from the stack and pushes `true` if they are equals, `false` otherwise.

### OP_CLOSURE (15)

Pushes a closure on the stack. Takes the next 2 bytes to get an index into the `strings` array to get the name.

**Warning**: doesn't capture anything at the moment.

### OP_POP (16)

Pops the top of the stack.

### OP_LOAD_MODULE_VAR (17)

Pushes on the stack a global variable. Takes the next 2 bytes to get an index into the `strings` array to get the name.

### OP_STORE_MODULE_VAR (18)

Pops on the stack and stores it in a global variable. Takes the next 2 bytes to get an index into the `strings` array to get the name.
Then pushes `null`.

### OP_CALL (19)

Takes the next 2 bytes to get an index into the `strings` array to get the name.\
Takes the next byte to know how many arguments are on the stack.

### OP_LOOP (20)

Identical to `OP_JUMP` but jumps backwards.

**Warning**: the offset is in `opcode` not in `byte`.

### OP_LOAD_LOCAL_VAR (21)

Takes the next 2 bytes to get the index of a local variable on the stack. And pushes it on the stack.

### OP_STORE_LOCAL_VAR (22)

Takes the next 2 bytes to get the index of a local variable on the stack.\
Pops the top of the stack and stores it in that local variable.\
Then pushes `null`.

### OP_ALLOCATE_VAR (23)

Takes the next 2 bytes to get an index into the `strings` array to get the type name.\
Pushes on the stack a variable of that type.

### OP_LOAD_FIELD_THIS (24)

Takes the next 2 bytes to get an index into the `strings` array to get the field name.\
Pushes on the stack the field of that name from the object at index 0 in the callframe.

### OP_STORE_FIELD_THIS (25)

Takes the next 2 bytes to get an index into the `strings` array to get the field name.\
Pops off the stack a variable and stores it in the field of that name in the object at index 0 in the callframe.

### OP_JUMP_IF (26)

The next byte is a forwards offset to jump to.Jumps if the top value of the stack is `true`.

**Warning**: the offset is in `opcode` not in `byte`.

### OP_JUMP (27)

The next byte is a forwards offset to jump to.

**Warning**: the offset is in `opcode` not in `byte`.

### OP_DUP (28)

Duplicates the top value of the stack.

### OP_LOOP_IF (29)

Identical to `OP_JUMP_IF` but jumps backwards.

**Warning**: the offset is in `opcode` not in `byte`.

### OP_IMPORT_MODULE (30)

The first 2 bytes are an index into the `strings` array to get the filename to import (`.rock` is automatically added).\
The next 2 bytes are the number of arguments.\
Each argument has that format:

* 2 bytes for the name in the symbol in the imported file, that is an index into the `strings` array.
* 2 bytes for the name for the symbol once in the importer file, that is an index into the `strings` array. (if you don't want to rename the symbol, set both to the same index)

### OP_SUPER (31)

Identical to `OP_CALL` except it calls the same named method in the parent class.

### OP_DUMP_STACK (255)

Dump the current stack.
