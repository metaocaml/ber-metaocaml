/* A small glue file to compile MetaOCaml code

   All the compilation is done by the patched ocamlc, and this program
   merely executes it.

   The purpose of this program is to set program options 
   to add compiler-libs/ and the compiler libraries -- but only when
   we link the final executable.  Otherwise,
   the final executable ends up with two copies of the compiler libraries,
   and this is the disaster, leading to sigfaults (because the the compiler
   librtaries, esp. env, are stateful).

   This program expects the preprocessor flag -DBINDIR for the path to ocamlc.
*/
  

#if !defined(BINDIR)
#error "Define BINDIR, as -DBINDIR=\"path-to-ocaml-bin-directory\""
#endif

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <stdio.h>

static char path_ocamlc [] = BINDIR  "/ocamlc";

/* New flags that we prepend to the command line */
static char *const metaocaml_extra_flags [] = {
 "-I", "+compiler-libs", 
 "ocamlcommon.cma", 
 "ocamlbytecomp.cma",
 "ocamltoplevel.cma",
 "metalib.cma", 
 "berstart.cmo"
   };

/* Flags that prevent building the executable */
static char *const nonlinking_flags [] = {
  "-c", "-pack", "-a", "-output-obj"
    };
 
/* Check to see that we are invoked to build the executable */
static int is_linking(char * const argv[])
{
  int i,j;
  for(i=1; argv[i]; i++)  /* argv is NULL-terminated */
    for(j=0; j<sizeof(nonlinking_flags)/sizeof(nonlinking_flags[0]); j++)
      if(strcmp(argv[i], nonlinking_flags[j]) == 0)
        return 0;
  return 1;
}

int main(int argc, char *const argv[],char *const envp[])
{
  char *const * new_argv = argv;

  if( is_linking(argv) ) {
    char **p;
    const int argv_len = argc+1; /* counting the NULL at the end of argv */
    const int extra_flags_len = sizeof(metaocaml_extra_flags)/
                                sizeof(metaocaml_extra_flags[0]);
    assert(new_argv=calloc(argv_len+extra_flags_len,sizeof(argv[0])));
    p = (char **)new_argv;
    *p++ = argv[0];
    memcpy(p,metaocaml_extra_flags,extra_flags_len*sizeof(argv[0]));
    p += extra_flags_len;
    memcpy(p,&argv[1],(argv_len-1)*sizeof(argv[0]));
  }
 
  execve(path_ocamlc,new_argv,envp); /* Does not normally return */
  perror("execve failed, perhaps wrong path to ocamlc");
  exit(2);
}
