// equality test, false branch



void main()
{
  int i = [0,9], t = [-5,4]; input:
  int k = 5;
  if (i >= 9) k = 20; else k = 2;
  k += t;
  assert(k<=5);
}
