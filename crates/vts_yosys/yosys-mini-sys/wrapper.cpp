#include "wrapper.h"

#include "rtlil.h"

YosysDesignPtr yosys_design_new() {
  return reinterpret_cast<void *>(new yosys_mini::RTLIL::Design);
}

void yosys_design_delete(YosysDesignPtr design_ref) {
  if (design_ref != nullptr) {
    auto design = reinterpret_cast<yosys_mini::RTLIL::Design *>(design_ref);
    delete design;
  }
}
