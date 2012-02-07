#include <stdio.h>

#include "erl_nif.h"
#include "dlcbf.h"

static ErlNifResourceType* dlcbf_RESOURCE;

typedef struct {
    dlcbf* dlcbf;
} dlcbf_handle;

ERL_NIF_TERM dlcbf_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int d, b;
    if (!enif_get_uint(env, argv[0], &d) || !enif_get_uint(env, argv[1], &b))
        return enif_make_badarg(env);

    dlcbf_handle* handle = (dlcbf_handle*)enif_alloc_resource(dlcbf_RESOURCE,
                                                            sizeof(dlcbf_handle));
    dlcbf* dlcbf = init((unsigned int)d, (unsigned int)b);
    handle->dlcbf = dlcbf;

    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

ERL_NIF_TERM dlcbf_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary data;

    dlcbf_handle* handle;
    if (!enif_get_resource(env, argv[1], dlcbf_RESOURCE, (void**)&handle) ||
        !enif_inspect_binary(env, argv[0], &data))
        return enif_make_badarg(env);

    dlcbf* dlcbf = handle->dlcbf;
    add(data.data, data.size, dlcbf);
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM in(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary data;
    dlcbf_handle* handle;
    if (!enif_get_resource(env, argv[1], dlcbf_RESOURCE, (void**)&handle) ||
        !enif_inspect_binary(env, argv[0], &data))
        return enif_make_badarg(env);

        dlcbf* dlcbf = handle->dlcbf;
        if (member(data.data, data.size, dlcbf)) {
            return enif_make_atom(env, "true");
        }
        else
        {
            return enif_make_atom(env, "false");
        }
}

static void dlcbf_resource_cleanup(ErlNifEnv* env, void* arg) {
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    dlcbf_RESOURCE = enif_open_resource_type(env,
                                            "dlcbf",
                                            "dlcbf_resource",
                                            &dlcbf_resource_cleanup,
                                            flags,
                                            0);

    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"new", 2, dlcbf_new},
    {"add", 2, dlcbf_insert},
    {"in", 2, in}
};

ERL_NIF_INIT(dlcbf, nif_funcs, &on_load, NULL, NULL, NULL);

