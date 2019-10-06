// inequality test

// the backward analysis finds i in [3;17], which lead to executions
// entering either branch


void main()
{
  int i = [0,19]; input:
  int k = 5;
  if (i >= 10) {
    k = k + i - 2;
  }
  else {
    k = k + i + 2;
  }
  assert(k>=10);
}

