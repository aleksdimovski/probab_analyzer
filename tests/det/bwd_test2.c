// inequality test

// the backward analysis finds i in [0;10], selecting only executions that
// enter the then branch


void main()
{
  int i = [0,19]; input:
  int k=0;
  if (i>=10) {
    k = 12;
  }
  else {
    k = 50;
  }
  assert(k>=30);
}
