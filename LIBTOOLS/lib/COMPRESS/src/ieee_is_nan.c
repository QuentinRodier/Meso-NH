#include <math.h>

#ifdef NO_UNDERSCORE
# define IEEE_IS_NAN ieee_is_nan
#else
# define IEEE_IS_NAN ieee_is_nan_
#endif

int IEEE_IS_NAN(double *x){
    return isnan(*x);
}
