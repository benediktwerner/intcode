# Intcode compiler, assembler and VM

Compiler, assembler and VM for the [intcode computer](https://adventofcode.com/2019/day/9)
from Advent of Code 2019. This is mainly for fun and to try out some different Rust parsing
libraries.

There are four crates in this project:

- `intcode`: Wrapper binary for executing the compiler, assembler or VM
- `vm`: An intcode VM that can run intcode files: `intcode run input.int`
- `asm`: An intcode assembler that can assemble intcode programs from intcode assembly: `intcode asm input.asm`
- `compiler`: An intcode compiler that can compile intcode programs from a higher-level language: `intcode compile input.asm`

## Building and Installing

Building or installing requires a working [Rust Installation](https://www.rust-lang.org/).

```
$ cargo install --path intcode
$ intcode
```

To build from source:

```
$ git clone https://github.com/benediktwerner/intcode
$ cd intcode
$ cargo build
$ ./target/debug/intcode
```

## Compiler

The compiler can compile code written in a simple high-level language to intcode:

```
// Comment

var x;              // Variables must be declared before their first use. Globals are initialized to zero.
var y = 13;         // but they can also be declared on the first assignment

var z = input();    // Get input
print(y);           // Produce output

func fib(x) {       // Arguments and variables declared in functions are seperate for each call
    y = 42;         // Modify a global variabl
    if x < 2 {
        return 1;
    }
    return fib(x - 1) * x;  // Recursion is possible
}

print(fib(z));
print(x);           // Still zero because the function has its own scope
print(y);           // Changed to 42
```

An extension for syntax highlighting in vscode can be found in [`vscode-syntax-highlighting`](vscode-syntax-highlighting).

## Assembler

### Example

This program computes Day 1 Part 1 in intcode:

```
# Comments start with '#'
start:                  # Label for jump
    in x                # Read to memory location x. The assembler automatically 'allocates' this memory after the program.
    eq x 0 tmp          # Check if input == 0. If yes, stop and print the result.
    jmp_true tmp :end   # Label targets must be prefixed with a ':' (to get the address instead of the value)
    div x 3 x
    sub x 2 x
    add total x total
    jmp :start

end:
    out total
    hlt

# Initialize 'total' to 0. The assembler does this automatically so
# this isn't really neccessary, but it shows the concept.
total: data 0
```

More examples can be found in the `examples` directory.

### Instructions

|        Operation         |                   Effect                   |                          Note                           |
| :----------------------: | :----------------------------------------: | :-----------------------------------------------------: |
|      `mov a target`      |                `target = a`                |                                                         |
|     `add a b target`     |              `target = a + b`              |                                                         |
|     `sub a b target`     |              `target = a - b`              |                                                         |
|     `mul a b target`     |              `target = a * b`              |                                                         |
|     `div a b target`     |             `target = a // b`              | Can be quite slow, only works for positive numbers atm. |
|     `mod a b target`     |              `target = a % b`              | Can be quite slow, only works for positive numbers atm  |
| `divmod a b target rest` |       `target, rest = divmod(a, b)`        | Can be quite slow, only works for positive numbers atm  |
|       `in target`        |             `target = input()`             |                                                         |
|         `out a`          |                 `print(a)`                 |                                                         |
|       `jmp target`       |               `goto target`                |                                                         |
|      `jnz a target`      |          `if a != 0: goto target`          |                     Alias: `jtrue`                      |
|      `jz a target`       |          `if a == 0: goto target`          |                     Alias: `jfalse`                     |
|     `eq a b target`      |             `target = a == b`              |                                                         |
|     `lt a b target`      |              `target = a < b`              |                                                         |
|     `leq a b target`     |             `target = a <= b`              |                                                         |
|     `gt a b target`      |              `target = a > b`              |                                                         |
|     `geq a b target`     |             `target = a >= b`              |                                                         |
|     `and a b target`     |             `target = a and b`             |                                                         |
|     `or a b target`      |             `target = a and b`             |                                                         |
|      `not a target`      |              `target = not a`              |                                                         |
|     `add_rel_base a`     |              `rel_base += a`               |                                                         |
|     `load a target`      |            `target = memory[a]`            |                                                         |
|     `store a target`     |            `memory[target] = a`            |                                                         |
|          `hlt`           |                  `exit()`                  |                      Alias: `halt`                      |
|         `data x`         |           stores `x` as raw data           |    Accepts multiple arguments, e.g. `data 1 5 13 42`    |
|     `array val len`      |    stores `val` `len` times as raw data    |    Accepts multiple arguments, e.g. `data 1 5 13 42`    |
|        `push val`        |  `memory[rel_base] = val; rel_base += 1`   |                                                         |
|       `pop target`       | `rel_base -= 1; target = memory[rel_base]` |                                                         |
|      `call target`       |                 `push(ip)`                 |                                                         |
|          `ret`           |               `goto pop(ip)`               |                                                         |

The predifined label `__end` can be used to get the address after all the generated code.
This is useful for putting a stack after the program: `add_rel_base :__end`. Simply putting
a label at the end of the program will not work if the program contains
undeclared labels/variables because they will be put after the program.

### Parameter types

- Identifier positional: `some_name`
- Identifier immediate: `:some_name`
- Identifier relative: `%some_name`
- Value positional: `[42]`
- Value immediate: `42`
- Value relative: `%42`
