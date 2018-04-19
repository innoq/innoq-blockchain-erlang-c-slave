#include <stdlib.h>
#include <string.h>
#include <erl_nif.h>
#include <openssl/sha.h>

static ERL_NIF_TERM find_block_parts_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary prefix, suffix;

  if (enif_inspect_binary(env, argv[0], &prefix)
      && enif_inspect_binary(env, argv[1], &suffix)) {
    unsigned char byteResultHash[SHA256_DIGEST_LENGTH];
    unsigned char proof[11];
    SHA256_CTX c;

    snprintf(proof, 11, "%d", 1917336);
    int proof_len = strlen(proof);

    SHA256_Init(&c);
    SHA256_Update(&c, prefix.data, prefix.size);
    SHA256_Update(&c, &proof, proof_len);
    SHA256_Update(&c, suffix.data, suffix.size);

    //Finalize and get the hash data
    SHA256_Final(byteResultHash, &c);

    ERL_NIF_TERM proof_term, hash_term;

    unsigned char *proof_data = enif_make_new_binary(env, proof_len, &proof_term);
    memcpy(proof_data, &proof, proof_len);
    unsigned char *hash_data = enif_make_new_binary(env, SHA256_DIGEST_LENGTH, &hash_term);
    memcpy(hash_data, &byteResultHash, SHA256_DIGEST_LENGTH);
    return enif_make_tuple(env, 3, enif_make_atom(env, "true"), proof_term, hash_term);
  }
  return enif_make_tuple(env, 1, enif_make_atom(env, "false"));
}

static ErlNifFunc nif_funcs[] = {
    {"find_block_parts", 5, find_block_parts_nif},
};

ERL_NIF_INIT(block_finder, nif_funcs, NULL, NULL, NULL, NULL)

