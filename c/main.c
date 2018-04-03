#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>


static void
call_orcml_run (char * prog)
{
  CAMLparam0 () ;
  CAMLlocal2 (oprog, ores) ;

  oprog = caml_copy_string (prog);

  value * func = caml_named_value ("orcml_run") ;

  if (func == NULL)
    puts ("orcml_run failed!") ;
  else
    ores = caml_callback (*func, oprog) ;

  printf ("Ocaml returned : '%s'\n", String_val (ores)) ;

  CAMLreturn0 ;
}


int
main (int argc, char ** argv)
{
  char * prog ;

  prog = argv [1] ;

  call_orcml_run (prog) ;

  return 0 ;
} /* main */
