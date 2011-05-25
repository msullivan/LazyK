/*
 * Brainfuck-to-Lazy K translation program.
 * Usage: obvious I think.
 * Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.
 */

#include <stdio.h>

int main(void)
{
  puts("````s``s`ks``s``s`ks``s`k`s``s``si`kk`ki```ss`si`kkkk```sii``s`k``s`k`s``si`k``s``si`k`kk`k``````sii```sii``s``s`kski``s`k`s``si`k`kik`kk`kik``sii");
  for (;;) {
    switch (getchar()) {
    case '+':
      puts("```s``s`ks``s`k`si``s`kk``s``s`ks``s`k`s`ks``s`k`s`kk``si`kk`k`k``s``s`k``s``s`ks``s`k`s``si`kk``s`k`s``s`ks``s`k`sik``s`kkk``s``s`ks``s`kk``s`ks``s`k`sik`kk``s`k`s``si`k`ki```ss`si`kk``s``si`k`ki`k`ki``s`kk``si`k`ki");
      break;
    case '-':
      puts("```s``s`ks``s`k`si``s`kk``s``s`ks``s`k`s`ks``s`k`s`kk``si`kk`k`k``si`k``s``s`ks``s``s`ks```ss`si`kk``s`k`s``s`ks``s`k`si``s`kk``si`k`ki``s`kk``s`kk``s`k`s``si`k`kik``s``s`ks``s`kk``s`ks``s`k`si``s`kk``si`k`ki`k``s`kk``s`k`s``si`k`kik``s`kk``si`k`ki");
      break;
    case '<':
      puts("```s``s`ks``s`k`si``s`kk``s``s`ks``s`k`s`ks``s`k`s`kk``s`k`s`ks``s`k`s`k`s`ks``s`k`s`k`s`kk``s``s`ks``s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`kk``si`kk`k`k``si`kk`k`k``si`k`ki`k`k```ss`s``s`ks``s`kk``s`ks``s`k`sik`kk``s`kk``si`k`ki");
      break;
    case '>':
      puts("```s``s`ks``s`k`si``s`kk``s``s`ks``s`k`s`ks``s`k`s`k`s`ks``s`k`s`k`s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`kk``s`k`s`ks``s`k`s`kk``s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`kk``si`kk`k`k``si`kk`k`k``s`k`s`kk``s``s`ks``s`kk``s`ks``s`k`sik`kk`k`k`k`k``si`k`ki``s`kk``si`k`ki");
      break;
    case '[':
      puts("```s``s`ks``s`k`si``s`kk``s`k`s`k``sii``s``s`ks``s`k`s`ks``s`k`s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`kk``s`k`s`ks``s`k`s`k`s``s``si`kk`kk``s`k`s`kk``s``si`k`ki`kk``s`kk``s`k`s`kk``s``s`ks```ss`si`kk`k``sii`k`k`ki``s`kk``s``si`k`ki`k`ki");
      break;
    case ']':
      puts("```s`k`s``si`kik");
      break;
    case ',':
      puts("```s``s`ks``s`k`si``s`kk``s`k`s`kk``s``s`ks``s`k`s`ks``s`k`s`k`s`ks``s`k`s`k`s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`k`s`ks``s`k`s`k`s`kk``s`k`s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`kk``s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`kk``si`kk`k`k``s``s`ks``s`k`si``s`kk``s``s``si`kk`k``s`k`s``si`k`kik`k`kk``s`kk``s``s``si`kk`k``si`k`ki`k``````sii```sii``s``s`kski``s`k`s``si`k`kik`kk`ki`k`kk`k`k`kk`k`k`k`k``si`k`ki``s`kk``si`k`ki");
      break;
    case '.':
      puts("```s``s`ks``s`k`si``s`kk``s`k`s`k`s``s`ks``s`kk``s`ks``s`kk``s`ks```ss`s``s`ks``s`kk``s`ks``s`k`si``s`kk``s`k```sii``s`k`s``s``si`kk`k`ki``s`k`s`k`s``s`ksk``s``s`ks``s`kk``sii`k``si`k`ki``si`kk`kk``si`kk``s`kk``si`k`ki");
      break;
    case EOF:
      puts("`k`k`k`k`k`k`k```sii```sii``s``s`kski");
      return 0;
    }
  }
}
