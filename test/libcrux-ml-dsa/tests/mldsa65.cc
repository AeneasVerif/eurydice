/*
 *    Copyright 2023 Cryspen Sarl
 *
 *    Licensed under the Apache License, Version 2.0 or MIT.
 *    - http://www.apache.org/licenses/LICENSE-2.0
 *    - http://opensource.org/licenses/MIT
 */

#include <gtest/gtest.h>

#include <fstream>
#include <nlohmann/json.hpp>
#include <vector>

#include "libcrux_mldsa65_portable.h"

using namespace std;

typedef vector<uint8_t> bytes;

Eurydice_borrow_slice_u8 mk_borrow_slice_u8(const uint8_t *x, size_t len) {
  Eurydice_borrow_slice_u8 s = {0};
  s.ptr = x;
  s.meta = len;
  return s;
}

template <typename T>
Eurydice_slice mk_slice(T *x, size_t len) {
  Eurydice_slice s = {0};
  s.ptr = (void *)x;
  s.len = len;
  return s;
}

TEST(MlDsa65TestPortable, ConsistencyTest) {
  // Generate key pair
  Eurydice_arr_60 keygen_rand = {0};
  memset(keygen_rand.data, 0x13, 32);

  Eurydice_arr_d10 signing_key = {0};
  Eurydice_arr_4a verification_key = {0};
  libcrux_ml_dsa_ml_dsa_65_portable_generate_key_pair_mut(
      keygen_rand, &signing_key, &verification_key);

  // Sign
  uint8_t msg[79] = {0};
  Eurydice_arr_60 sign_rand = {0};
  memset(sign_rand.data, 0x13, 32);
  uint8_t context[3] = {0};

  auto msg_slice = mk_borrow_slice_u8((uint8_t *)msg, 79);
  auto context_slice = mk_borrow_slice_u8((uint8_t *)context, 3);
  Eurydice_arr_96 signature = {0};
  auto signature_result = libcrux_ml_dsa_ml_dsa_65_portable_sign_mut(
      &signing_key, msg_slice, context_slice, sign_rand, &signature);
  EXPECT_EQ(signature_result.tag, Ok);

  // Verify
  auto result = libcrux_ml_dsa_ml_dsa_65_portable_verify(
      &verification_key, msg_slice, context_slice, &signature);

  EXPECT_EQ(result.tag, Ok);
}

#ifdef LIBCRUX_X64
#include "libcrux_mldsa65_avx2.h"

TEST(MlDsa65TestAvx2, ConsistencyTest) {
  Eurydice_arr_60 keygen_rand = {0};
  memset(keygen_rand.data, 0x13, 32);
  auto key_pair = libcrux_ml_dsa_ml_dsa_65_avx2_generate_key_pair(keygen_rand);

  // Sign
  uint8_t msg[79] = {0};
  Eurydice_arr_60 sign_rand = {0};
  memset(sign_rand.data, 0x13, 32);
  uint8_t context[3] = {0};

  auto msg_slice = mk_borrow_slice_u8((uint8_t *)msg, 79);
  auto context_slice = mk_borrow_slice_u8((uint8_t *)context, 3);
  auto signature_result = libcrux_ml_dsa_ml_dsa_65_avx2_sign(
      &key_pair.signing_key, msg_slice, context_slice, sign_rand);
  EXPECT_EQ(signature_result.tag, Ok);
  auto signature = signature_result.val.case_Ok;

  // Verify
  auto result = libcrux_ml_dsa_ml_dsa_65_avx2_verify(
      &key_pair.verification_key, msg_slice, context_slice, &signature);

  EXPECT_EQ(result.tag, Ok);
}
#endif  // LIBCRUX_X64

class KAT {
 public:
  bytes key_generation_seed;
  bytes sha3_256_hash_of_verification_key;
  bytes sha3_256_hash_of_signing_key;
  bytes message;
  bytes signing_randomness;
  bytes sha3_256_hash_of_signature;
};

vector<uint8_t> from_hex(const string &hex) {
  if (hex.length() % 2 == 1) {
    throw invalid_argument("Odd-length hex string");
  }

  int len = static_cast<int>(hex.length()) / 2;
  vector<uint8_t> out(len);
  for (int i = 0; i < len; i += 1) {
    string byte = hex.substr(2 * i, 2);
    out[i] = static_cast<uint8_t>(strtol(byte.c_str(), nullptr, 16));
  }

  return out;
}

string bytes_to_hex(const vector<uint8_t> &data) {
  stringstream hex(ios_base::out);
  hex.flags(ios::hex);
  for (const auto &byte : data) {
    hex << setw(2) << setfill('0') << int(byte);
  }
  return hex.str();
}

string bytes_to_hex(const uint8_t *data, size_t len) {
  stringstream hex(ios_base::out);
  hex.flags(ios::hex);
  for (size_t i = 0; i < len; ++i) {
    hex << setw(2) << setfill('0') << int(data[i]);
  }
  return hex.str();
}

vector<KAT> read_kats(string path) {
  ifstream kat_file(path);
  nlohmann::json kats_raw;
  kat_file >> kats_raw;

  vector<KAT> kats;

  // Read test group
  for (auto &kat_raw : kats_raw.items()) {
    auto kat_raw_value = kat_raw.value();

    kats.push_back(KAT{
        from_hex(kat_raw_value["key_generation_seed"]),
        from_hex(kat_raw_value["sha3_256_hash_of_verification_key"]),
        from_hex(kat_raw_value["sha3_256_hash_of_signing_key"]),
        from_hex(kat_raw_value["message"]),
        from_hex(kat_raw_value["signing_randomness"]),
        from_hex(kat_raw_value["sha3_256_hash_of_signature"]),
    });
  }

  return kats;
}

TEST(MlDsa65TestPortable, NISTKnownAnswerTest) {
  // XXX: This should be done in a portable way.
  auto kats = read_kats("tests/nistkats-65.json");

  Eurydice_arr_60 keygen_rand = {0};
  Eurydice_arr_60 sign_rand = {0};

  for (auto kat : kats) {
    // Generate key pair
    memcpy(keygen_rand.data, kat.key_generation_seed.data(), 32);

    Eurydice_arr_d10 signing_key = {0};
    Eurydice_arr_4a verification_key = {0};
    libcrux_ml_dsa_ml_dsa_65_portable_generate_key_pair_mut(
        keygen_rand, &signing_key, &verification_key);

    auto vk_hash =
        libcrux_sha3_sha256(mk_borrow_slice_u8(verification_key.data, 1952U));
    EXPECT_EQ(0, memcmp(vk_hash.data,
                        kat.sha3_256_hash_of_verification_key.data(), 32));

    auto sk_hash =
        libcrux_sha3_sha256(mk_borrow_slice_u8(signing_key.data, 4032U));
    EXPECT_EQ(
        0, memcmp(sk_hash.data, kat.sha3_256_hash_of_signing_key.data(), 32));

    // Sign
    memcpy(sign_rand.data, kat.signing_randomness.data(), 32);
    Eurydice_borrow_slice_u8 context = {0};

    auto msg_slice = mk_borrow_slice_u8(kat.message.data(), kat.message.size());
    Eurydice_arr_96 signature = {0};
    auto signature_result = libcrux_ml_dsa_ml_dsa_65_portable_sign_mut(
        &signing_key, msg_slice, context, sign_rand, &signature);
    EXPECT_EQ(signature_result.tag, Ok);

    auto sig_hash =
        libcrux_sha3_sha256(mk_borrow_slice_u8(signature.data, 3309U));
    EXPECT_EQ(0,
              memcmp(sig_hash.data, kat.sha3_256_hash_of_signature.data(), 32));

    // Verify
    auto result = libcrux_ml_dsa_ml_dsa_65_portable_verify(
        &verification_key, msg_slice, context, &signature);
    EXPECT_EQ(result.tag, Ok);
  }
}

#ifdef LIBCRUX_X64
TEST(MlDsa65TestAvx2, NISTKnownAnswerTest) {
  // XXX: This should be done in a portable way.
  auto kats = read_kats("tests/nistkats-65.json");

  Eurydice_arr_60 keygen_rand = {0};
  Eurydice_arr_60 sign_rand = {0};

  for (auto kat : kats) {
    // Generate key pair
    memcpy(keygen_rand.data, kat.key_generation_seed.data(), 32);

    Eurydice_arr_d10 signing_key = {0};
    Eurydice_arr_4a verification_key = {0};
    libcrux_ml_dsa_ml_dsa_65_avx2_generate_key_pair_mut(
        keygen_rand, &signing_key, &verification_key);

    auto vk_hash =
        libcrux_sha3_sha256(mk_borrow_slice_u8(verification_key.data, 1952U));
    EXPECT_EQ(0, memcmp(vk_hash.data,
                        kat.sha3_256_hash_of_verification_key.data(), 32));

    auto sk_hash =
        libcrux_sha3_sha256(mk_borrow_slice_u8(signing_key.data, 4032U));
    EXPECT_EQ(
        0, memcmp(sk_hash.data, kat.sha3_256_hash_of_signing_key.data(), 32));

    // Sign
    memcpy(sign_rand.data, kat.signing_randomness.data(), 32);
    Eurydice_borrow_slice_u8 context = {0};

    auto msg_slice = mk_borrow_slice_u8(kat.message.data(), kat.message.size());
    Eurydice_arr_96 signature = {0};
    auto signature_result = libcrux_ml_dsa_ml_dsa_65_avx2_sign_mut(
        &signing_key, msg_slice, context, sign_rand, &signature);
    EXPECT_EQ(signature_result.tag, Ok);

    auto sig_hash =
        libcrux_sha3_sha256(mk_borrow_slice_u8(signature.data, 3309U));
    EXPECT_EQ(0,
              memcmp(sig_hash.data, kat.sha3_256_hash_of_signature.data(), 32));

    // Verify
    auto result = libcrux_ml_dsa_ml_dsa_65_avx2_verify(
        &verification_key, msg_slice, context, &signature);
    EXPECT_EQ(result.tag, Ok);
  }
}
#endif  // #ifdef LIBCRUX_X64
