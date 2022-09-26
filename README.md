# Borax

Parser and interpreter for the c. 1970 programming language B.

B was developed at Bell Labs by Ken Thompson and Dennis M. Ritchie. The
target application was system software for minicomputers.

Certain features were incorporated from *Bell Laboratories Computing Science
Technical Report #8: The Programming Language B* (1973) (Brian Kernighan, Steve Johnson).
Namely

- Bitwise complement operator `~`
- `break;` statement performs the equivalent of a goto out of nearest
switch or while statement.
- Automatic vector declarations use square brackets to indicate size.
- Vectors declared with size N actually hold N+1 words. The bonus 1 word
of space can be ignored or not. In particular `vect[N]` is not an error.

However some features eliminated in the later document were retained.

- Octal numeric constants may include 8s and 9s, i.e. `09` = `011`.

Code specimen are taken from *Users' Reference to B* (1972) by Ken Thompson.

## Example

```
main() {
  extrn printf, getchar, time, exit;
  auto timev[2];

  printf("What is my task...*n");
  printf("o - print Hello World*n");
  printf("t - print system time*n");
  printf("x - exit immediately*n");
  printf("> ");

  switch getchar() {
    case 'o':
      printf("Hello World!*n");
      goto done;
    case 't':
      time(timev);
      printf("The system time is (%d,%d)*n", timev[0], timev[1]);
      goto done;
    case 'x':
      printf("As you wish--");
      exit();
      goto done;
  }

  done: printf("End of Program*n");
}
```

