/*
 * Automatically generated buffer error
 *
 * Instantiates: AccessAttr.write, BoundaryAttr.below, LocationAttr.stack, MagnitudeAttr.small, DataSizeAttr.huge, ExcursionAttr.continuous
 *
*/


/*TEMPLATE-PARAMS*
 *
 * DATA_TYPES = ['int', 'float', 'double', 'char']
 * BUF_TYPES = ['array', 'ptr']
 * MIN_ID_LEN, MAX_ID_LEN = (1, 10)
 * MIN_ARGS, MAX_ARGS = (0, 4)
 * sizeof = {'int': 4, 'float': 4, 'double': 8, 'char': 1}
 * BUF_ACCESS = ['ARRAY_ACCESS', 'PTR_ACCESS']
 * RW = ['READ', 'WRITE']
 * LOCATION = ['STACK', 'HEAP']
 *
 */

// READ_WRITE = (READ, WRITE)
#define WRITE
// BUF_ACCESS = (PTR_ACCESS, ARRAY_ACCESS)
#define PTR_ACCESS
// LOCATION = (STACK, HEAP)
#define STACK

// BUF2 = (BUF2_PRESENT, BUF2_ABSENT)
#define BUF2_ABSENT

// ARG_TYPE = (FIXED_ARGS, VARIADIC_ARGS)
#define FIXED_ARGS

// Required for malloc's definition
#ifdef HEAP
    #include <stdlib.h>
#endif

// TAIL_CALL = (TAIL_CALL, NO_TAIL_CALL)
#ifdef tail_call
#endif


#ifdef FIXED_ARGS
char piOdJldLVG (int EJoJfIGmlj, float toDhWio, char Cef, float Un)
#else //VARIADIC_ARGS
/*
char piOdJldLVG (const char* format, ...)

  va_list argp;
  va_start(argp, format);
  while (*format != '\0') {
    if (*format == '%') {
      format++;
      if (*format == '%') {
        putchar('%');
      } else if (*format == 'c') {
        char char_to_print = va_arg(argp, int);
        putchar(char_to_print);
      } else {
        fputs("Not implemented", stdout);
      }
    } else {
      putchar(*format);
    }
    format++;
  }
  va_end(argp);
*/
#endif
{
    /*
     * Declarations
     */
    char UEdydtGV;
    double XTUyz;

    //Initialization? memset?
#ifdef STACK
    double sDDhScGx[71688] = {0};
    /*
     * What about a stack pointer?
     * buf_type buf_[N]; buf_type* buf = buf_; Any different?
     */
#else //HEAP
    //TODO: is it always going to be sizeof(type)*N?
    double* sDDhScGx = malloc(sizeof(double)*71688);
#endif

#ifdef BUF2
#ifdef STACK
     [] = {0};
#else //HEAP
    *  = malloc(sizeof(double)*);
#endif

#endif

    /*
     * Buffer Access: Read/Write
     */
    for(int i=66316; i>=(66316+-66365); i=i+-1)
    {
#ifdef READ
    #ifdef PTR_ACCESS
         XTUyz = *(sDDhScGx + i);
    #else //ARRAY_ACCESS
         XTUyz = sDDhScGx[i];
    #endif
#else //WRITE
    #ifdef PTR_ACCESS
         *(sDDhScGx + i) = XTUyz;
    #else //ARRAY_ACCESS
         sDDhScGx[i] = XTUyz;
    #endif
#endif
    }
    // TODO: Return what value?
    return UEdydtGV;
}

int main(int argc, char *argv[])
{

    int EJoJfIGmlj; float toDhWio; char Cef; float Un;
    ;
    char prfygr;

    /*
     * fun call: parameterize stack depth?
     * fun ptr?
     * using the buffer as a ptr?
     * smash main's stack instead? Is it any different from smashing another
     * function's stack? Is main's stack special?
     */
    prfygr = piOdJldLVG(EJoJfIGmlj, toDhWio, Cef, Un);
    return 0;
}
