
#include <stdio.h>

main()
{
  printf("%s W%drld!\n", "Hello", 0); // Hello W0rld!
  printf("%s W%drld!\n", 0, "Hello"); // (null) W134514152rld!
  printf("%s W%drld!\n", "Hello"); // Hello W134514152rld!
  int x = 0;
  printf("%Y\n", x); // Y
}
