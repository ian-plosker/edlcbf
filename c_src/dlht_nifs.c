#include <stdlib.h>

#include "erl_nif.h"
#include "erl_driver.h"
#include "dlht.h"

ERL_NIF_TERM new(ErlNifEnv* env, ErlNifUInt64 d, ErlNifUInt64 b) {
    dlht = enif_alloc_resource();
    enif_make_resource();
    return init(d, b);
}

ERL_NIF_TERM insert(ErlNifEnv* env, ErlNifBinary bin, ERL_NIF_TERM dlht) {
    add(bin.data, bin.size, dlht);

    return dlht;
}

static ErlNifFunc nif_funcs[] =
{
    {"new", 2, new},
    {"add", 2, add}
};

ERL_NIF_INIT(dlht_nifs, nif_funcs, NULL, NULL, NULL, NULL)

