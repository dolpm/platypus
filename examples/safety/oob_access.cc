#include <iostream>
#include <vector>
using namespace std;

int main() {
  vector<int> v;
  for (int i = 0; i < 20; i += 1) {
    v.push_back(i);
  }

  int *last = &v.back();

  cout << *last << endl;

  v.clear();
  v.shrink_to_fit();

  cout << *last << endl;

  return 0;
}