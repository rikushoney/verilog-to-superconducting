#include "wrapper.h"

#include "kernel/rtlil.h"

YosysDesignPtr yosys_new_design() {
  return reinterpret_cast<void *>(new Yosys::RTLIL::Design);
}

void yosys_delete_design(YosysDesignPtr design_ref) {
  if (design_ref != nullptr) {
    auto design = reinterpret_cast<Yosys::RTLIL::Design *>(design_ref);
    delete design;
  }
}
