#include <string.h>
#include <erl_nif.h>

static ERL_NIF_TERM find_block_parts_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM result;
  unsigned char *data = enif_make_new_binary(env, 18, &result);
  memcpy(data, "Hello world from C", 18);
  return enif_make_tuple(env, 3, enif_make_atom(env, "true"), result, result);
}

static ErlNifFunc nif_funcs[] = {
    {"find_block_parts", 5, find_block_parts_nif},
};

ERL_NIF_INIT(block_finder, nif_funcs, NULL, NULL, NULL, NULL)

