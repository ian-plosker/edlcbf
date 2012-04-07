#include <stdio.h>

#include "erl_nif.h"
#include "dlcbf.h"

static ErlNifResourceType* dlcbf_RESOURCE;

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_OK;

typedef struct {
    Dlcbf* dlcbf;
} DlcbfHandle;

ERL_NIF_TERM
dlcbf_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int d, b;
    if (!enif_get_uint(env, argv[0], &d) || !enif_get_uint(env, argv[1], &b)) {
        return enif_make_badarg(env);
    }

    DlcbfHandle* handle = (DlcbfHandle*)enif_alloc_resource(dlcbf_RESOURCE, sizeof(DlcbfHandle));
    Dlcbf* dlcbf = dlcbf_init(d, b);
    handle->dlcbf = dlcbf;

    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);

    return enif_make_tuple2(env, ATOM_OK, result);
}

ERL_NIF_TERM
dlcbf_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;

    DlcbfHandle* handle;
    if (!enif_get_resource(env, argv[1], dlcbf_RESOURCE, (void**)&handle) ||
        !enif_inspect_binary(env, argv[0], &data)) {
        return enif_make_badarg(env);
    }

    Dlcbf* dlcbf = handle->dlcbf;
    dlcbf_add(data.data, data.size, dlcbf);
    return ATOM_OK;
}

ERL_NIF_TERM
dlcbf_in(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    DlcbfHandle* handle;
    if (!enif_get_resource(env, argv[1], dlcbf_RESOURCE, (void**)&handle) ||
        !enif_inspect_binary(env, argv[0], &data)) {
        return enif_make_badarg(env);
    }

    Dlcbf* dlcbf = handle->dlcbf;
    return dlcbf_member(data.data, data.size, dlcbf) ? ATOM_TRUE : ATOM_FALSE;
}

static void
dlcbf_resource_cleanup(ErlNifEnv* env, void* arg)
{
    DlcbfHandle* handle = (DlcbfHandle*)arg;
    dlcbf_destroy(handle->dlcbf);
}

static int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    dlcbf_RESOURCE = enif_open_resource_type(env,
                                             "dlcbf",
                                             "dlcbf_resource",
                                             &dlcbf_resource_cleanup,
                                             flags,
                                             0);
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_OK = enif_make_atom(env, "ok");
    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"new", 2, dlcbf_new},
    {"add", 2, dlcbf_insert},
    {"in", 2, dlcbf_in},
};

ERL_NIF_INIT(dlcbf, nif_funcs, &on_load, NULL, NULL, NULL);

