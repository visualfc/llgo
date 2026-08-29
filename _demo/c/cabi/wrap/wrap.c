#include <stdint.h>

typedef struct {
    int8_t tag;
    int64_t value;
} register_pair;

register_pair roundtrip_pair(register_pair input) {
    input.tag++;
    input.value += 2;
    return input;
}

typedef struct {
    float values[9];
} large_result;

large_result make_large(float base) {
    large_result result;
    for (int i = 0; i < 9; i++) {
        result.values[i] = base + i;
    }
    return result;
}

typedef struct {
    int32_t values[4];
} callback_value;

typedef callback_value (*aggregate_callback)(callback_value);

callback_value call_callback(aggregate_callback callback, callback_value value) {
    return callback(value);
}

extern callback_value go_export(callback_value);

callback_value call_go_export(callback_value value) {
    return go_export(value);
}
