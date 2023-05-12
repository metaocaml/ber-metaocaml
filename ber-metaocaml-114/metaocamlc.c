/* A small glue file to compile MetaOCaml code (bytecode or native)

   All the compilation is done by the patched ocamlc/ocamlopt, and this program
   merely executes it.

   The purpose of this program is to set program options,
   including the options to add compiler-libs/ and the compiler libraries.
   The library options are added however only when we link the final 
   executable.  Otherwise,
   the final executable ends up with two copies of the compiler libraries,
   and this is the disaster, leading to sigfaults (because the the compiler
   librtaries, esp. env, are stateful).

   This program expects the preprocessor flags:
     -DOPT    for the native compiler
     -DBINDIR for the path to ocamlc/ocamlopt
*/
  

#if !defined(BINDIR)
#error "Define BINDIR, as -DBINDIR=\"path-to-ocaml-bin-directory\""
#endif

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <stdio.h>

#if defined(OPT)
static char path_ocamlc [] = BINDIR  "/ocamlopt";
#else
static char path_ocamlc [] = BINDIR  "/ocamlc";
#endif

/* New flags that we prepend to the command line always */
static char *const metaocaml_extra_flags [] = {
 "-short-paths", "-open", "Codelib"
};
static const int extra_flags_len = 
  sizeof(metaocaml_extra_flags)/sizeof(metaocaml_extra_flags[0]);

/* New flags that we prepend to the command line when building executable */
#if defined(OPT)
static char *const metaocaml_extra_flags_exe [] = {
 "-I", "+compiler-libs", 
 "ocamlcommon.cmxa",
 "dynlink.cmxa",
 "codelib.cmx",
 "runnative.cmx"
   };
#else
static char *const metaocaml_extra_flags_exe [] = {
 "-I", "+compiler-libs", 
 "ocamlcommon.cma", 
 "ocamlbytecomp.cma",
 "ocamltoplevel.cma",
 "metalib.cma", 
 "berstart.cmo"
   };
#endif
static const int extra_flags_exe_len = 
  sizeof(metaocaml_extra_flags_exe)/sizeof(metaocaml_extra_flags_exe[0]);

/* Flags that prevent building the executable */
static char *const nonlinking_flags [] = {
  "-c", "-pack", "-shared", "-a", "-output-obj"
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
  const int linking = is_linking(argv);
  const int argv_len = argc+1; /* counting the NULL at the end of argv */
  char *const * new_argv = argv;

  {
    const int new_flags_len = extra_flags_len +
      ( linking ? extra_flags_exe_len : 0);
    char **p;
    assert((new_argv=calloc(argv_len+new_flags_len,sizeof(argv[0]))));
    p = (char **)new_argv;
    *p++ = argv[0];
    memcpy(p,metaocaml_extra_flags,extra_flags_len*sizeof(argv[0]));
    p += extra_flags_len;
    if (linking ) {
      memcpy(p,metaocaml_extra_flags_exe,extra_flags_exe_len*sizeof(argv[0]));
      p += extra_flags_exe_len;
    }
    memcpy(p,&argv[1],(argv_len-1)*sizeof(argv[0]));
  }
 
  execve(path_ocamlc,new_argv,envp); /* Does not normally return */
  perror("execve failed, perhaps wrong path to ocamlc/ocamlopt");
  exit(2);
}
