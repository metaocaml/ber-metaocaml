/* More precise timing functions and CPU counters */

#include "cycle.h"		/* Get cycle counter, FFTW project */ 
				/* http://www.fftw.org/download.html */

/* The following is probablyu no longer needed: caml_sys_time
   now uses rusage
*/

/* Return the structure of user and system process times, in whole
   and fractional seconds.
   sys_times:: unit -> double * double
   Because we invoke caml_copy_double() function, which may cause gc(),
   we must use CAMLparam/CAMLreturn() conventions.
*/
CAMLprim value sys_times(value unit)
{
  CAMLparam0 ();   /* unit is unused */
  struct rusage ru;
  CAMLlocal3 (result, utime, stime);

  memset(&ru,0,sizeof ru);
  getrusage(0,&ru);
  result = caml_alloc_small (2, 0);
  utime = caml_copy_double((double)ru.ru_utime.tv_sec + 
		      (double)ru.ru_utime.tv_usec/1000000.0);
  stime = caml_copy_double((double)ru.ru_stime.tv_sec + 
		      (double)ru.ru_stime.tv_usec/1000000.0);
  Field(result, 0) = utime;  /* Simple assignments don't cause GC */
  Field(result, 1) = stime;  /* We're overriding freshly allocated fields */
  CAMLreturn (result);
}

/* Ticks is an abstract data type. 
   It means different things on different systems.
   See cycle.h for more details.
 */

/*
 Get the current ticks counter. Return an abstract value
*/

CAMLprim value sys_get_ticks(value unit)
{
  value res;
  ticks t;
  Assert(sizeof(t) == (sizeof t/sizeof res) * sizeof(res)); /* a constant op*/
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, (sizeof t/sizeof res), Abstract_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  t = getticks();
  memcpy(Bp_val(res),&t,sizeof t);
  return res;
}

/*
 * Return the difference between the current tick counter and the passed one.
 */
CAMLprim value sys_elapsed_ticks(value t0)
{
  /* no CAML memory allocation, except for the very end */
  ticks tt0, tt1 = getticks();
  memcpy(&tt0,Bp_val(t0),sizeof tt0);
  return caml_copy_double(elapsed(tt1,tt0));
}
