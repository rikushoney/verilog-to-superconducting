#pragma once

#ifdef __cplusplus
extern "C" {
#endif

typedef void *YosysDesignPtr;

YosysDesignPtr yosys_new_design();
void yosys_delete_design(YosysDesignPtr design_ref);

#ifdef __cplusplus
}
#endif
