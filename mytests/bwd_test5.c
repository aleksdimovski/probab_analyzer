// equality test, true branch


void main()
{
  int i = [0,19], t = [-9,0]; input:
  int k = 5;
  if (i < 10) k = 10; else k = 2;
  k += t;
  assert(k>=5);
}
