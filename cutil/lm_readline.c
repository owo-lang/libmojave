/*cxxx
 * Readline Interface for MCC
 * Copyright(c) 2002 Justin David Smith, Caltech
 * Derived from MetaPRL readline code
 */


/*
 * Call the readline package.
 */
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifdef READLINE
#include <caml/callback.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <errno.h>
#endif


/*
 * Max line length without readline.
 */
#define MAX_LINE_LENGTH         1024


/***  Readline Tab Completion Stubs  ***/


#ifdef READLINE

static const value *command_callback_f = NULL;

/*
 * Attempt command-name completion (assuming a list of
 * commands have been registered into the system).
 */
static char **command_completion(const char *text, int start, int end) {

   CAMLparam0();
   CAMLlocal2(words,str);

   /* disable file pathname completion */
   rl_attempted_completion_over = 1;

   str = caml_copy_string(text);
   words = caml_callback(*command_callback_f, str);

   CAMLdrop;

   int n = Wosize_val(words);
   if (n == 0) return(NULL);

   char **matches = malloc((1 + n) * sizeof(char*));
   if (matches == NULL) return(matches);
   int i;
   for (i = 0; i < n; ++i) {
       const char *s = String_val(Field(words, i));
       size_t len = 1 + strlen(s);
       char *cs = malloc(len);
       matches[i] = strcpy(cs, s);
   }
   matches[n] = NULL;

   return(matches);

}


/*
 * Actually initialize readline.
 */
value caml_initialize_readline(value unit) {

   CAMLparam1(unit);

   /* Setup callback */
   command_callback_f = caml_named_value("readline completion");

   /* Tell the completer about our command completion engine */
   rl_attempted_completion_function = *command_completion;

   /* Set horizontal scroll mode; other modes screw up display */
   rl_variable_bind("horizontal-scroll-mode", "on");

   /* Disable the bell */
   rl_variable_bind("bell-style", "none");

   CAMLreturn(Val_unit);

}


value caml_read_history(value name) {

    CAMLparam1(name);
    int result;
    result = read_history( String_val(name) );
    if (result == ENOENT) {
        raise_not_found();
    }
    else if (result != 0) {
        CAMLlocal1(error);
        error = copy_string(strerror( result ));
        raise_sys_error( error );
    }
    CAMLreturn(Val_unit);

}

value caml_write_history(value name) {

    CAMLparam1(name);
    int result;
    result = write_history( String_val(name) );
    if (result != 0) {
        CAMLlocal1(error);
        error = copy_string(strerror( result ));
        raise_sys_error( error );
    }
    CAMLreturn(Val_unit);

}

value caml_history_truncate_file(value name, value nlines) {

    CAMLparam2(name, nlines);
    int result;
    result = history_truncate_file( String_val(name), Long_val(nlines) );
    if (result != 0) {
        CAMLlocal1(error);
        error = copy_string(strerror( result ));
        raise_sys_error( error );
    }
    CAMLreturn(Val_unit);

}




#else /* No READLINE... */


/*
 * Nothing to initialize :)
 */
value caml_initialize_readline(value unit) {

   CAMLparam1(unit);
   CAMLreturn(Val_unit);

}

value caml_read_history(value name) {

   CAMLparam1(name);
   CAMLreturn(Val_unit);

}
value caml_write_history(value name) {

   CAMLparam1(name);
   CAMLreturn(Val_unit);

}
value caml_history_truncate_file(value name, value len) {

   CAMLparam2(name,len);
   CAMLreturn(Val_unit);

}

#endif /* READLINE enabled? */


/***  OCaml Interface  ***/


/*
 * Read a line into a string buffer.
 * Returns a string option, None at EOF.
 */
value caml_readline(value prompt_arg) {

   CAMLparam1(prompt_arg);
   CAMLlocal2(v, b);
   char *line;

#ifdef READLINE

   line = readline(String_val(prompt_arg));

   /* Readline returns null on EOF */
   if(line == NULL) {
      /* None */
      CAMLreturn(Val_int(0));
   }

   /* This (probably) copies the line */
   if(line != NULL && *line != '\0') {
      /* Add nonempty lines to the history */
      add_history(line);
   }

#else /* No READLINE */

   char *bufp;

   bufp = malloc(MAX_LINE_LENGTH);
   if(bufp == NULL) {
      /* Pretend that we have reached EOF */
      CAMLreturn(Val_int(0));
   }

   /* Get the line (make sure string is terminated) */
   bufp[MAX_LINE_LENGTH - 1] = '\0';
   fputs(String_val(prompt_arg), stdout);
   fflush(stdout);
   line = fgets(bufp, MAX_LINE_LENGTH - 1, stdin);

   /* Readline returns null on EOF */
   if(line == NULL) {
      /* None */
      free(bufp);
      CAMLreturn(Val_int(0));
   }

#endif /* READLINE enabled? */

   /* Copy the line */
   v = copy_string(line);

   /* Some v */
   b = alloc(1, 0);
   Field(b, 0) = v;

   /* Free the buffer */
   free(line);

   CAMLreturn(b);

}
