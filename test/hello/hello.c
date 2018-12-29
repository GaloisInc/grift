#include "unistd.h"

int main()
{
  write(STDIN_FILENO, "Hi\n", 3);
}
