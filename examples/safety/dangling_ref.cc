#include <iostream>
using namespace std;

int *talk_to_saul(int *goodmans_cut, int *legal_fees) {
  if (*goodmans_cut > *legal_fees) {
    cout << "Business is booming!" << endl;
    return goodmans_cut;
  }
  cout << "Guy's gotta eat... sorry fellas" << endl;
  return legal_fees;
}

void silly_function_to_overwrite_stack() { return; }

int *heisenberg(int *methlymine, bool *ricin, int *pseudoephedrine) {
  int desired_quantity_ml = 1500;
  int goodmans_cut = (desired_quantity_ml / 20) * 1000;
  if (*methlymine + *pseudoephedrine < desired_quantity_ml) {
    int *gc = &goodmans_cut;
    int *tts = talk_to_saul(gc, methlymine);

    cout << "JESSE! We don't have enough to cook!" << endl;
    return tts;
  } else {
    cout << "Let's cook Jesse." << endl;
    if (*ricin) {
      return methlymine;
    }
    return pseudoephedrine;
  }
}

int main() {
  int madrigal_supplies = 1000;
  {
    int *train_heist = &madrigal_supplies;
    int pseudo_needed = 100;
    {
      int *from_the_hardware_store = &pseudo_needed;
      bool tuco_angry = true;
      int *blue_sky =
          heisenberg(train_heist, &tuco_angry, from_the_hardware_store);

      silly_function_to_overwrite_stack();

      if (*blue_sky != 75000) {
        cout << "Oh no! Undefined behaviour! ";
      } else {
        cout << "All looks good! ";
      }

      cout << "expected: 75000, recieved: " << *blue_sky << endl;
    }
  }
  return 0;
}