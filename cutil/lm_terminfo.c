/*
 * Simple Terminfo Interface for ML
 * Copyright(c) 2002 Justin David Smith, Caltech
 */
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#ifdef WIN32
#   pragma warning (disable: 4127 4189 4702 4996)
#endif

#ifdef NCURSES

/* Headers that are readline-specific must be included here. */
#include <ncurses.h>

#ifdef TERMH_IN_NCURSES
#  include <ncurses/term.h>
#else
#  include <term.h>
#endif

static int loaded_terminfo = 0;

static int load_terminfo() {

   char *termname = NULL;

   /* Check to see if we already loaded the terminal data */
   if(loaded_terminfo) return(0);

   /* We haven't loaded anything yet (or we had an error). */
   if(setupterm(NULL, 1, NULL) == OK) {
      /* We were successful! */
      loaded_terminfo = 1;
      return(0);
   }

   /* Failure. */
   return(-1);

}

/* Converts termcap data type to capname */
static char *capnames[] = {
    "bold", "snrmq", "ssubm", "ssupm", "smul", "sitm", "sgr0"
};

#endif /* NCURSES support? */

/*
 * Terminfo is enabled only of TERM is defined.
 */
value caml_tgetstr_enabled(value unit)
{
   CAMLparam1(unit);
   CAMLreturn(getenv("TERM") ? Val_true : Val_false);
}


/*
 * Read the indicated terminfo by string.
 */
value caml_tgetstr(value id) {

   CAMLparam1(id);
   CAMLlocal1(result);
   char *termdata = NULL;

   /* Lookup the requested capability name.  Note that we only get terminfo
      if we compiled with readline support; otherwise it will not be linked
      in.  */
#ifdef NCURSES
   if(load_terminfo() == 0) {
       char *name = capnames[Int_val(id)];
       termdata = tigetstr(name);
   }
#endif /* NCURSES */

   /* Note that tigetstr will return either 0 or -1 on error. */
   if(termdata == NULL || termdata == (char *)(-1)) {
      result = Val_int(0); /* None */
   } else {
      result = caml_alloc_small(1, 0); /* Some string */
      Field(result, 0) = caml_copy_string(termdata);
      /* apparently we're not supposed to free termdata here */
      /* TEMP:  I cannot find specs on this! */
      //free(termdata);
   }

   /* Return the result */
   CAMLreturn(result);

}
