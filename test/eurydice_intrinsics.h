#if defined(__aarch64__)
#include <arm_neon.h>
typedef uint16x8_t Vec128;
#elif defined(__i686__)
typedef __m128i Vec128;
#else
#error "Incorrect header inclusion"
#endif
