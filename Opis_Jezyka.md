# JPP | Zadanie 2 | interpreter

### Nazwa języka
`Jabba the Lang`
### Rozszerzenie plików
`.jbb`

## Specyfikacja języka

Typ języka: imperatywny

Język nie posiada szczególnych udziwnień w porównaniu do Latte.

### Typy, zmienne, stałe, literały, przekazywanie argumentów

Języka trzy typy: `Integer`, `Boolean`, `String`. Każda może być zadeklarowana jako `var` lub `val`. Pierwsza z nich oznacza zmienną możliwą do modyfikacji, druga oznacza stałą. Wszystkie zmienne są inicjalizowane wartością domyślną. `Integer` = `0`, `Boolean` = `false`, `String` = `""`. Wszystkie zmienne są typowane statycznie. W przypadku próby przypisania zmiennej typu `Integer` wartości typu `String` zostanie zgłoszony wyjątek.

Każdy z trzech typów posiada literały. `Integer` - liczby całkowite, `Boolean` - `true` i `false`, `String` - ciągi znaków w cudzysłowach.

Na liczbach można wykonywać operacje arytmetyczne: `+`, `-`, `*`, `/`, `%`. Na `Boolean` można wykonywać operacje logiczne: `&&`, `||`, `!`. Na `String` można wykonywać operacje konkatenacji: `+`. Na `Integer` można wykonywać operacje porównania: `==`, `!=`, `<`, `>`, `<=`, `>=`.

Standardowe przekazania zmiennej do funkcji odbywa się przez referencję (nie kopię referencji). 
Przykład:
```kotlin
fun foo(var x: Integer) : Unit {
    x = 10;
}
var y: Integer = 5;
foo(y);
// x = 10, ponieważ x zostało przekazane przez referencję
```
Przy czym gdyby `y` było typu `val`, to wtedy program zgłosiłby błąd na etapie sprawdzania typów.
Aby przekazać kopię należy dopisać słowo kluczowe `new` przed typem.
Przykład:
```kotlin
fun foo(var x: new Integer) : Unit {
    x = 10;
}
var y: Integer = 5;
foo(y);
// x = 5, ponieważ x zostało przekazane przez wartość
```

Wymusza to na użytkowniku zastanowienia się przed skopiowaniem zmiennej, ponieważ jest to kosztowna operacja (może nie w przypadku naszych prostych typów, ale w ogólności).

W przypadku zwykłych operacji na zmiennych zawsze dochodzi do kopiowania. Nie pozwalamy na referencje poza wywołaniami funkcji. Przykład:
```kotlin
var x: Integer = 5;
var y: Integer = x;
y = 10;
// x = 5, ponieważ x zostało przekazane przez wartość
```

## Funkcje standardowe (w tym print)

```kotlin
// wypisuje `str` na stdout
writeStr(str: String) : Unit

// wypisuje `num` na stdout
writeInt(num: Integer) : Unit

// zamienia zmienną typu `Integer` na `String`
toString(Integer) : String

// zamienia zmienną typu `String` na `Integer`
toInt(String) : Integer

// przechodzi o jeden krok w generatorze i zwraca wartość
//  rzuca wyjątek w przypadku przejścia przez całe generator
next(Gen[Integer]) : Integer
next(Gen[Boolean]) : Boolean
next(Gen[String]) : String

// sprawdza czy generator ma następny krok
hasNext(Gen[T]) : Boolean
```

### Instrukcje warunkowe, pętle, break, continue, while-finally
Język posiada instrukcje warunkowe `if` (`if else`) oraz pętle `while` oraz `for`. Każda z instrukcji wymaga bloku kodu `{}`,
ale nie wymaga nawiasów okrągłych `()`. Przykład:
```kotlin
if true {
    writeInt(5);
}
// wyświetli 5

if true {
    writeInt(5);
} else {
    writeInt(10);
}
// wyświetli 5

while x < 10 {
    writeInt(x);
    x = x + 1;
} // wyświetli 5, 6, 7, 8, 9
```
W bloku pętli `while` oraz `for` można dodać instrukcję `break` lub `continue`.
W przypadku pętli `while` można dodać blok `finally` na końcu, który wykona się po wyjściu
z pętli, jeżeli nie wystąpiło wywołanie instrukcji `break`.
 Przykład:
```kotlin
while x < 10 {
    if x == 5 {
        break;
    }
    writeInt(x);
    x = x + 1;
} // wyświetli 0, 1, 2, 3, 4

x = 0;
while x < 2 {
    if x == 1 {
        break;
    }
    x = x + 1;
} finally {
    writeStr("finally");
} // nic nie wyświetli

x = 0;
while x < 2 {
    if x == 10 {
        break;
    }
    x = x + 1;
} finally {
    writeStr("finally");
} // wyświetli "finally"
```

Pętle `for` są dwóch rodzajów. Range loop oraz generator loop. Range loop wygląda następująco:
```kotlin
for x in 0..10 {
    writeInt(x);
} // wyświetli 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```
Pętla `for x in e1..e2` jest równoważna pętli `while e1 <= e2 { x = e1; e1 = e1 + 1; }`. Stała `x` jest typu `Integer` i jest widoczna tylko wewnątrz pętli. 

Generator loop wygląda następująco:
```kotlin
for x in gen() {
    writeInt(x);
} // wyświetli 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```
Pętla `for x in gen()` jest równoważna pętli `val g: Gen[T] = gen(); while hasNext(g) { x = next(g); }`. Stała `x` jest typu `T` i jest widoczna tylko wewnątrz pętli, gdzie T to typ zwracany przez generator g.

### Funkcje

Funkcje można deklarować na dowolnym poziomie zagnieżdżenia. Funkcje mogą zwracać wartość lub nie. Funkcje mogą przyjmować dowolną liczbę argumentów. Przykład:
```kotlin
fun foo(val x: Integer, val y: Integer) : Integer {
    return x + y;
}

fun bar(val x: Integer, val y: Integer) : Unit {
    writeInt(x + y);
}
```
Widoczność zmiennych dla funkcji jest taka sama jak dla bloków kodu `{}`. Przykład:
```kotlin
fun foo(val x: Integer, val y: Integer) : Integer {
    var z: Integer = 5;
    return x + y + z;
}
// zmienna z jest widoczna tylko wewnątrz funkcji foo
```
```kotlin
fun foo(val x: Integer, val y: Integer) : Integer {
    var z: Integer = 5;
    fun bar(val x: Integer, val y: Integer) : Integer {
        return x + y + z; // z widoczny jest z funkcji foo; x, y przysłonięte 
    }
    return bar(x + 1, y + 1) + z;
}
foo(1, 2); // zwróci 15
```

### Błędy składni, typów i wykonania

Błędna składnia (parser, lexer) zostaną sprawdzone jako pierwsze. 
W przypadku błędu składni program nie zostanie uruchomiony.
Typy zostaną sprawdzone przed wykonaniem programu. Błędy typów to na przykład:
- przypisanie wartości złego typu do zmiennej (w tym literałów)
  - `var x: Integer = "abc";` albo `var x: Integer = 10; x = "abc";`
- próba zmiany stałej
  - `val c: Integer = 10; c = 20;`
- przekazanie błędnego typu do funkcji
  - `fun foo(val x: Integer) : Integer { return x; } foo("abc");`
- przekazanie złej liczby argumentów do funkcji
  - `fun foo(val x: Integer) : Integer { return x; } foo(1, 2);`
- przekazanie stałej w miejsce zmiennej do funkcji
  - `fun foo(var x: Integer) : Integer { return x; } val y: Integer = 10; foo(y);`
- sprawdzenie zwracanego typu przez funkcję
  - `fun foo(val x: Integer) : String { return x; }`
- sprawdzenie zwracanych typów przez generator
  - `fun foo(val x: Integer) : Gen[String] { yield x; }`
- użycie niezadeklarowanej zmiennej
  - `var x: Integer = 10; x = x + y;`

W trakcie wykonania programu mogą wystąpić błędy wykonania. Błędy wykonania to na przykład:
- dzielenie przez 0

### Generatory
Generatory są dodatkowym typem danych. Nie można na nim wykonywać żadnych operacji, nie można go kopiować,
przekazywać do funkcji. Generator można utworzyć tylko poprzez odpowiednio zdefiniowaną funkcję.
Na przykład generator liczb całkowitych:
```kotlin
fun range(val start: Integer, val end: Integer) : Gen[Integer] {
    var x: Integer = start;
    while x <= end {
        yield x;
        x = x + 1;
    }
}
```
Generator jest typu `Gen[T]`, gdzie `T` to typ zwracany przez generator.
Generator jest funkcją, która wykonuje się aż do momentu, w którym wywołana zostanie funkcja `yield e`.
Ponowane wywołania generatora rozpoczyna się od miejsca ostatniego zakończenia. 
Any sprawdzić czy generator się zakończyć (t.j. doszedł do końca bloku i nie ma więcej wywołań `yield`),
można użyć funkcji `hasNext(g)`, gdzie `g` to generator. Z kolei aby wywołać generator należy użyć funkcji `next(g)`.
Gdy generator dojdzie do końca, to kolejne wywołania `next(g)` będą domyślną wartość danego typu.
Na przykład:
```kotlin
fun two_stpes() : Gen[Integer] {
    yield 1;
    yield 2;
}

fun main() : Unit {
    var g: Gen[Integer] = two_steps();
    while hasNext(g) {
        writeInt(next(g));
    } // wyświetli 1, 2, 0
    // has next sprawdzi tylko czy generator się zakończył
}
// lub
fun main() : Unit {
    for x in two_steps() {
        writeInt(x);
    } // wyświetli 1, 2
    // for sprawdzi czy generator się zakończył.
    // W przeciwieństwie do ręcznego sprawdzenia hasNext,
    // for nie wywoła next, jeśli generator się zakończył.
    // nie potrafimy tego zrobić ręcznie, ponieważ
    // nie wiemy kiedy zakończy się generator.
    // W przypadku generatora `two_steps` najpierw zwróci on 1, potem 2, 
    // a potem będzie zwracał domyślną wartość dla typu Integer, czyli 0.
    // Nie wiemy jednak, że dany `yield` był ostatnio dopóki generator nie dojdzie 
    // do końca. Gdy to zrobi możemy zignorować ostatnią wartość `next(g)`.
    // For robi to za nas. While nie. 
}
```

### Dołączone pliki
Dodatkowe przykłady pozostają w katalogu `Lang/Examples/`.
Reguły składni są w pliku `Lang/jabba.cf`.

Na koniec przykład programu w języku Jabba:

```kotlin

fun foo_123(val str1: String, var num: new Integer) : Integer {
    writeStr(str1);

    // zwiększy kopię x o 1 i go zwróci
    num = num + 1;
    return num;
}

fun bar_abc(var num: Integer) : Unit {

    // zwiększy x dwukrotnie i zmieni oryginał
    num = num * 2;
}

fun main() : Unit {
    // x = -1
    var x: Integer = 10 - 11;

    // na stdout wpisze "abc", zwróci 0, zmienna x będzie miała wartość -1
    foo_123("abc", x);

    // po wykonaniu funkcji x będzie równa -2
    bar_abc(x);
}

// Wywołanie funkcji main (jak w Pythonie)
main();

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