# RWL
Redukcja wyrażeń logicznych
AGH IIET / Python / Projekt 1

Skrypt redukuje wyrażenia logiczne dane na standardowym wejściu.

Wyrażenie ma zawierać zmienne logiczne a-z, T i F ( prawda/fałsz), nawiasy '(' i ')' oraz operacje o danych priorytetach:

```
Priorytet 4:
~ negacja

Priorytet 3:
^ XOR

Priorytet 2:
& AND
| OR
/ NAND

Priorytet 1:
> implikacja
```

Przykład działania:

```
a|~a&(b|~b)|F
T

(a|~b|c)&(~a|b|c)
(a|b>a&b)|c

a>(b^d)
a>b^d

a^b>a/b
T

F>(a|b/c^d&e)
T

((((~a^b)&c)/d)&e)
~a^b&c/d&e

(~a&~b&~c)|(~a&b&~c)|(a&~b&c)|(a&b&~c)
a|c>a&~b&c|(b&~c)

kappa
ERROR
```