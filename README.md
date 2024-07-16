# What is this?

A terrible programming language built without any plan or idea behind it. 
Lots of silly code inside, so beware.

* Syntax - whatever I feel like playing around with at the time.

* Defined language semantics - no

* GC - I would like to try to implement a GC one day, so probably.

* How do I build and run the compiler? - please don't

* How do I run tests? - what tests?

## Why?

It's fun! Always wanted to learn about compiler development.


## Why this name?

It's just some boring old language without any special features or ideas, if it can even be called a programming language.


## What it looks like?
This program (kind of) compiles which is already a success for me.
```
Values :: struct
{
    a: i32;
    b: i32;
    c: i32;
}

GLOBAL_VALUE : i32 : 3;

main :: (): i32
{
    s := Values { 
        a: 11,
        b: 22, 
        c: 33,
    };

    d := if s.a == 3 {
        0
    } else {
        1
    };

    mutate :: () {
        s.a = 15;
        d = d - 5;
    }
    
    mutate();

    result1 : i32 = sum(s);

    printf("%d |", result1 + d);

    s.a = 94;
    s.b = 182;
    s.c = 138;

    result2 := sum(s);

    printf("| %d", result2 + GLOBAL_VALUE);

    0
}

sum :: (v: Values): i32
{
    v.a + v.b + v.c + GLOBAL_VALUE
}
```
