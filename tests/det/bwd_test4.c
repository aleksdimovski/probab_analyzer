// inequality test

// the backward analysis finds i in [6;13]



void main()
{
  int i = [0,19], j = [-1,2]; input:
  int k = 5;
  if (i >= 10) {
    k = k + i + j ;
  }
  else {
    k = k + i + j;
  }
  assert(k>=10);
}

