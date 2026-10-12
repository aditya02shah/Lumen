## Lumen
- A just-in-time (JIT) compiler for the *Lumen* programming language, implemented in OCaml
- Lumen resembles a lighter C, with support for optional types & immutable variables
- Lumen files use the `.sui` extension

## Features
- Tree-walk interpreter
- Stack-based bytecode virtual machine & intepreter
- JIT compilation to ARM64

## Architecture
- Source program -> Lexer -> Parser -> Semantic Checker -> Bytecode -> Virtual Machine -> ARM64 Machine Code → Native Execution

## Example

### Running program
- `dune exec lumen <lumen_file>`
- `examples/for.sui`:
Lumen Program:
```c
int sum_n(int n){
    mut int sum = 0;
    for (mut int i = 1; i <= n; i = i + 1){
        sum = sum + i;
    }

    return sum;
}

int main(){
    int sum = sum_n(30);
    return sum;
}
```
- Intermediate Bytecode:
```
0: PUSH_CONST 0                     
1: STORE_LOCAL 1
2: PUSH_CONST 1
3: STORE_LOCAL 2
4: LOAD_LOCAL 2
5: LOAD_LOCAL 0
6: BOOLEAN LESS_THAN_EQUALS
7: JMP 1 10
8: LOAD_LOCAL 1
9: LOAD_LOCAL 2
10: ARITHMETIC ADD
11: STORE_LOCAL 1
12: LOAD_LOCAL 2
13: PUSH_CONST 1
14: ARITHMETIC ADD
15: STORE_LOCAL 2
16: UNCONDITIONAL_JMP -12
17: LOAD_LOCAL 1
18: RET
19: CALL 0 [[PUSH_CONST 20]]
20: STORE_LOCAL 0
21: LOAD_LOCAL 0
22: RET
```
- Compiled executable if hotspot is detected:
```asm
0:        ff 03 08 d1     sub     sp, sp, #0x200
4:        e5 03 00 91     mov     x5, sp
8:        e4 a3 01 91     add     x4, sp, #0x68
c:        83 02 80 d2     mov     x3, #0x14
10:       a3 00 00 f9     str     x3, [x5]
14:       a3 00 40 f9     ldr     x3, [x5]
18:       83 00 00 f9     str     x3, [x4]
1c:       03 00 80 d2     mov     x3, #0x0
20:       a3 00 00 f9     str     x3, [x5]
24:       a3 00 40 f9     ldr     x3, [x5]
28:       83 04 00 f9     str     x3, [x4, #0x8]
2c:       23 00 80 d2     mov     x3, #0x1
30:       a3 00 00 f9     str     x3, [x5]
34:       a3 00 40 f9     ldr     x3, [x5]
38:       83 08 00 f9     str     x3, [x4, #0x10]
3c:       83 08 40 f9     ldr     x3, [x4, #0x10]
40:       a3 00 00 f9     str     x3, [x5]
44:       83 00 40 f9     ldr     x3, [x4]
48:       a3 04 00 f9     str     x3, [x5, #0x8]
4c:       a2 04 40 f9     ldr     x2, [x5, #0x8]
50:       a1 00 40 f9     ldr     x1, [x5]
54:       3f 00 02 eb     cmp     x1, x2
58:       ea c7 9f 9a     cset    x10, le
5c:       5f 05 00 f1     cmp     x10, #0x1
60:       40 00 00 54     b.eq    0x68
64:       16 00 00 14     b       0xbc
68:       83 04 40 f9     ldr     x3, [x4, #0x8]
6c:       a3 00 00 f9     str     x3, [x5]
70:       83 08 40 f9     ldr     x3, [x4, #0x10]
74:       a3 04 00 f9     str     x3, [x5, #0x8]
78:       a2 04 40 f9     ldr     x2, [x5, #0x8]
7c:       a1 00 40 f9     ldr     x1, [x5]
80:       23 00 02 8b     add     x3, x1, x2
84:       a3 00 00 f9     str     x3, [x5]
88:       a3 00 40 f9     ldr     x3, [x5]
8c:       83 04 00 f9     str     x3, [x4, #0x8]
90:       83 08 40 f9     ldr     x3, [x4, #0x10]
94:       a3 00 00 f9     str     x3, [x5]
98:       23 00 80 d2     mov     x3, #0x1
9c:       a3 04 00 f9     str     x3, [x5, #0x8]
a0:       a2 04 40 f9     ldr     x2, [x5, #0x8]
a4:       a1 00 40 f9     ldr     x1, [x5]
a8:       23 00 02 8b     add     x3, x1, x2
ac:       a3 00 00 f9     str     x3, [x5]
b0:       a3 00 40 f9     ldr     x3, [x5]
b4:       83 08 00 f9     str     x3, [x4, #0x10]
b8:       e1 ff ff 17     b       0x3c
bc:       83 04 40 f9     ldr     x3, [x4, #0x8]
c0:       a3 00 00 f9     str     x3, [x5]
c4:       a3 00 40 f9     ldr     x3, [x5]
c8:       e0 03 03 aa     mov     x0, x3
cc:       ff 03 08 91     add     sp, sp, #0x200
d0:       c0 03 5f d6     ret
d4:       a3 fc 7f f9     ldr     x3, [x5, #0x7ff8]
d8:       e0 03 03 aa     mov     x0, x3
dc:       ff 03 08 91     add     sp, sp, #0x200
e0:       c0 03 5f d6     ret
```


## Project Structure

```text
├── bin/                  # executable entry point
├── lib/
│   ├── lexer.ml          # converts text into Lumen tokens
│   ├── parser.ml         # processes tokens into AST
│   ├── ast.ml            # AST structure
│   ├── semantic_check.ml # type-checking
│   ├── interpreter.ml    # tree-walk interpreter for AST
│   ├── opcode.ml         # bytecode structure
│   ├── bytecode.ml       # AST to bytecode conversion
│   ├── vm.ml             # bytecode interpreter
│   ├── memory.c          # memory manipulation & native code execution
│   ├── jit.ml            # hotspot detection and JIT compilation
│   └── emit.ml           # ARM64 machine-code generation
```


Credit:
## References
- [ARM64 Instruction Set Cheat Sheet](https://www.cs.swarthmore.edu/~kwebb/cs31/resources/ARM64_Cheat_Sheet.pdf)