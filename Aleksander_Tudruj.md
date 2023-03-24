# JPP | Zadanie 2 | interpreter

## Specyfikacja języka

Typ języka: imperatywny

EBNF w pliku "lang/grammar.ebnf"

```example

fun foo_123(str1: String, new num: Integer) : Integer {
    write(str1);

    return num = num + 1;
}

fun bar_abc(num: Integer) : unit {
    num = num * 2;
}

fun main() : Unit {
    var x = 10 - 11;
    foo_123("abc", x);
    bar_abc(x);
}

main();

```

