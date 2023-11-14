#include <stdio.h>

#include "wrapper.h"

int main() {
  auto design = yosys_design_new();
  printf("allocated design at %p", design);
  yosys_design_delete(design);
  design = nullptr;
  printf("design deleted");
}
