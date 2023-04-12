struct Inner
{
  int data;
};

struct Outer
{
  struct Inner jewel;
};

int main()
{
  struct Inner baby = {10};
  struct Outer mother = {baby};

  return 0;
}