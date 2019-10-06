// loop with assert at exit


void main()
{
  int j = [0,10]; input:
  int i = 0;
  while (i < 100) {
    i++;
    j++;
  }
  assert(j >= 103 || j <= 108);
}
