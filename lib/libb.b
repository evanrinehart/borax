/* ================ */
/* Library Routines */
/* ================ */

/* Note: printn, printf and descriptions were copied
   directly from the B Users' reference. Other routines
   written in 2022. */

/* The i-th character of the string is returned. */

   char(str, i)
     return(127 & (str[i/5] >> 7*(4-i%5)));


/* The character c is stored in the i-th character of the string. */

   lchar(str, i, c) {
     auto wi, shift;
     wi      = i/5;
     sh      = 7*(4-i%5);
     str[wi] =& ~(127 << sh);
     str[wi] =|     c << sh ;
   }


/* The system time (60-ths of a second) represented in the
   two-word vector time is converted to a 16-character date in
   the 8-word vector date. The converted date has the follow-
   ing format: "Mmm dd hh:mm:ss". */

/* Note: borax is using 5 characters per word so date
   is at least a 4-word vector. */

   ctime(time, date) {
     /* Project for a later date */
     date[0] = 'Jan 0';
     date[1] = '1 00:';
     date[2] = '00:00';
     date[3] = '*e';
   }


/* The following function will print a non-negative number, n, to
   the base b, where 2<=b<=10. This routine uses the fact that
   in the ANSCII character set, the digits 0 to 9 have sequential
   code values. */

   printn(n,b) {
     extrn putchar;
     auto a;

     if(a=n/b) /* assignment, not test for equality */
       printn(a, b); /* recursive */
     putchar(n%b + '0');
   }

/* The following function is a general formatting, printing, and
   conversion subroutine. The first argument is a format string.
   Character sequences of the form '%x' are interpreted and cause
   conversion of type 'x' of the next argument, othercharacter
   sequences are printed verbatim.  Thus

      printf("delta is %d*n", delta);

   will convert the variable delta to decimal (%d) and print the
   string with the converted form of delta in place of %d.  The
   conversions %d-decimal, %o-octal, %s-string and %c-character
   are allowed.

   This program calls upon the function 'printn'. (see section
   9.1) */

   printf(fmt, x1,x2,x3,x4,x5,x6,x7,x8,x9) {
     extrn printn, char, putchar;
     auto adx, x, c, i, j;
     i = 0; /* fmt index */
     adx = &x1; /* argument pointer */
   loop:
     while((c=char(fmt,i++) ) != '%') {
       if(c == '*e')
         return;
       putchar(c);
     }
     x = *adx++;
     switch c = char(fmt,i++) {
     case 'd': /* decimal */
     case 'o': /* octal */
       if(x < 0) {
         x = -x ;
         putchar('-');
       }
       printn(x, c=='o'?8:10);
       goto loop;
     case 'c' : /* char */
       putchar(x);
       goto loop;
     case 's': /* string */
       j = 0;
       while((c=char(x, j++)) != '*e')
         putchar(c);
       goto loop;
     }
     putchar('%');
     i--;
     adx--;
     goto loop;
   }


/* ============ */
/* I/O wrappers */
/* ============ */

/* Note: I/O is implemented with memory mapped I/O. Writing
   to special addresses will be trapped by the runtime system
   and will force a service call. Input characters or updated
   system time can then be accessed normally. */

/* The character c is written on the standard output file. */

   putchar(c)
     *__IO.output = c;


/* The next character from the standard input file is re-
   turned.  The character '*e' is returned for an end-of-file. */

   getchar() {
     *__IO.input.query = 1;
     return (__IO.input.cache);
   }


/* The current process is terminated. */

   exit()
     *__IO.exit = 1;


/* The current system time is returned in the 2-word vector
   timev. */

   time(timev) {
     *__IO.time.query = 1;
     timev[0] = __IO.time.cachev[0];
     timev[1] = __IO.time.cachev[1];
   }

