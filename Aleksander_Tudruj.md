# JPP | Zadanie 2 | interpreter

### Nazwa języka
`Jabba the Lang`
### Rozszerzenie plików
`.jbb`

## Specyfikacja języka

Typ języka: imperatywny

EBNF w pliku "lang/grammar.ebnf"

Przykłady użycia w plikach "lang/examples/*.jbb"

```kotlin

fun foo_123(str1: String, new num: Integer) : Integer {
    writeStr(str1);

    // zwiększy kopię x o 1 i go zwróci
    return num = num + 1;
}

fun bar_abc(num: Integer) : unit {

    // zwiększy x dwukrotnie i zmieni oryginał
    num = num * 2;
}

fun main() : Unit {
    // x = -1
    var x = 10 - 11;

    // na stdout wpisze "abc", zwróci 0, zmienna x będzie miała wartość -1
    foo_123("abc", x);

    // po wykonaniu funkcji x będzie równa -2
    bar_abc(x);
}

// Wywołanie funkcji main (jak w Pythonie)
main();

```

## Funkcje standardowe

```kotlin
// wypisuje `str` na stdout
writeStr(str: String) : Unit

// wypisuje `num` na stdout
writeInt(num: Integer) : Unit

// wczytuje `str` z stdin
readStr() : String

// wczytuje `num` z stdin
readInt() : Integer

// zamienia zmienną typu `Integer` na `String`
toString(Integer) : String

// zamienia zmienną typu `String` na `Integer`
toInt(String) : Integer

// przechdzi o jeden krok w generatorze i zwraca wartość
//  rzuca wyjątek w przypadku przejścia przez całe generator
next(Gen[Integer]) : Integer
next(Gen[Boolean]) : Boolean
next(Gen[String]) : Strign

// sprawdza czy generator ma następny krok
hasNext(Gen) : Boolean
```

## Tabelka deklaracji
```txt
  Na 15 punktów
  01 (trzy typy)
  02 (literały, arytmetyka, porównania)
  03 (zmienne, przypisanie)
  04 (print)
  05 (while, if)
  06 (funkcje lub procedury, rekurencja)
  07 (przez zmienną / przez wartość)
  08 (zmienne read-only i pętla for)
  Na 20 punktów
  09 (przesłanianie i statyczne wiązanie)
  10 (obsługa błędów wykonania)
  11 (funkcje zwracające wartość)
  Na 30 punktów
  12 (4) (statyczne typowanie)
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  16 (1) (break, continue)
  18 (3) (generatory)

Razem: 30
```

