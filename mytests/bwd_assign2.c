// non-invertible assignment

// requires add_space_var (refines add_space) to maximize k before forget
// in assign



void main()
{
  int j = [1,2], i = [10,19]; input:
  int k;
  k = i + j;
  assert(k>=15);
}
