#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <erl_nif.h>
#include <openssl/evp.h>

#define MAX_NUMBER_LEN 25

static ERL_NIF_TERM find_block_parts_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary prefix, suffix;
  unsigned long from, to, leading_zeros;

  if (!enif_inspect_binary(env, argv[0], &prefix)
      || !enif_inspect_binary(env, argv[1], &suffix)
      || !enif_get_ulong(env, argv[2], &from)
      || !enif_get_ulong(env, argv[3], &to)
      || !enif_get_ulong(env, argv[4], &leading_zeros)) {
    return enif_make_badarg(env);
  }

  leading_zeros /= 2;

  unsigned char zeros[leading_zeros];
  char proof[MAX_NUMBER_LEN];
  memset(zeros, 0, leading_zeros);

  unsigned int md_len;
  unsigned char byteResultHash[EVP_MAX_MD_SIZE];
  const EVP_MD *md = EVP_sha256();
  EVP_MD_CTX *mdctx_prefix = EVP_MD_CTX_create();
  EVP_MD_CTX *mdctx = EVP_MD_CTX_create();

  EVP_DigestInit_ex(mdctx_prefix, md, NULL);
  EVP_DigestUpdate(mdctx_prefix, prefix.data, prefix.size);

  for (long i = from; i < to; i++) {
    snprintf(proof, MAX_NUMBER_LEN, "%ld", i);
    int proof_len = strlen(proof);

    EVP_MD_CTX_copy_ex(mdctx, mdctx_prefix);
    EVP_DigestUpdate(mdctx, &proof, proof_len);
    EVP_DigestUpdate(mdctx, suffix.data, suffix.size);
    EVP_DigestFinal_ex(mdctx, byteResultHash, &md_len);

    if (!memcmp(byteResultHash, zeros, leading_zeros)) {
      EVP_MD_CTX_destroy(mdctx_prefix);
      EVP_MD_CTX_destroy(mdctx);
      ERL_NIF_TERM proof_term, hash_term;

      unsigned char *proof_data = enif_make_new_binary(env, proof_len, &proof_term);
      memcpy(proof_data, &proof, proof_len);
      unsigned char *hash_data = enif_make_new_binary(env, md_len, &hash_term);
      memcpy(hash_data, &byteResultHash, md_len);
      return enif_make_tuple(env, 3, enif_make_atom(env, "true"), proof_term, hash_term);
    }
  }
  EVP_MD_CTX_destroy(mdctx_prefix);
  EVP_MD_CTX_destroy(mdctx);
  return enif_make_tuple(env, 1, enif_make_atom(env, "false"));
}

static ErlNifFunc nif_funcs[] = {
    {"find_block_parts", 5, find_block_parts_nif},
};

ERL_NIF_INIT(block_finder, nif_funcs, NULL, NULL, NULL, NULL)

