#pragma once

#ifdef __cplusplus
extern "C" {
#endif

typedef void *YosysDesignPtr;

YosysDesignPtr yosys_design_new();
void yosys_design_delete(YosysDesignPtr design_ref);

#ifdef __cplusplus
}
#endif
