#include <stdlib.h>
#include <string.h>
#include <erl_nif.h>
#include <openssl/sha.h>

#define MAX_NUMBER_LEN 11

static ERL_NIF_TERM find_block_parts_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary prefix, suffix;
  unsigned int from, to, leading_zeros;

  if (!enif_inspect_binary(env, argv[0], &prefix)
      || !enif_inspect_binary(env, argv[1], &suffix)
      || !enif_get_uint(env, argv[2], &from)
      || !enif_get_uint(env, argv[3], &to)
      || !enif_get_uint(env, argv[4], &leading_zeros)) {
    return enif_make_badarg(env);
  }

  leading_zeros /= 2;

  unsigned char zeros[leading_zeros];
  unsigned char proof[MAX_NUMBER_LEN];
  memset(zeros, 0, leading_zeros);

  unsigned char byteResultHash[SHA256_DIGEST_LENGTH];
  SHA256_CTX c;

  for (int i = from; i < to; i++) {
    snprintf(proof, MAX_NUMBER_LEN, "%d", i);
    int proof_len = strlen(proof);

    SHA256_Init(&c);
    SHA256_Update(&c, prefix.data, prefix.size);
    SHA256_Update(&c, &proof, proof_len);
    SHA256_Update(&c, suffix.data, suffix.size);
    SHA256_Final(byteResultHash, &c);

    if (!memcmp(byteResultHash, zeros, leading_zeros)) {
      ERL_NIF_TERM proof_term, hash_term;

      unsigned char *proof_data = enif_make_new_binary(env, proof_len, &proof_term);
      memcpy(proof_data, &proof, proof_len);
      unsigned char *hash_data = enif_make_new_binary(env, SHA256_DIGEST_LENGTH, &hash_term);
      memcpy(hash_data, &byteResultHash, SHA256_DIGEST_LENGTH);
      return enif_make_tuple(env, 3, enif_make_atom(env, "true"), proof_term, hash_term);
    }
  }
  return enif_make_tuple(env, 1, enif_make_atom(env, "false"));
}

static ErlNifFunc nif_funcs[] = {
    {"find_block_parts", 5, find_block_parts_nif},
};

ERL_NIF_INIT(block_finder, nif_funcs, NULL, NULL, NULL, NULL)

