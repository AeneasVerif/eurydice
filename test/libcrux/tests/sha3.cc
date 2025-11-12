
#include <gtest/gtest.h>
#include <nlohmann/json.hpp>

#include "libcrux_sha3_portable.h"
#include "libcrux_mlkem768.h"

using namespace std;

typedef vector<uint8_t> bytes;

vector<uint8_t>
from_hex(const string& hex)
{
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

string
bytes_to_hex(const vector<uint8_t>& data)
{
  stringstream hex(ios_base::out);
  hex.flags(ios::hex);
  for (const auto& byte : data) {
    hex << setw(2) << setfill('0') << int(byte);
  }
  return hex.str();
}

TEST(Sha3Test, ConsistencyTest)
{
    const char* message = "Hello, World!";
    uint32_t message_size = strlen(message);

    Eurydice_borrow_slice_u8 input;
    input.ptr = (uint8_t*) message;
    input.meta = message_size;

    auto digest = libcrux_sha3_sha256(input);

    bytes expected_digest = from_hex(
      "1af17a664e3fa8e419b8ba05c2a173169df76162a5a286e0c405b460d478f7ef");

    EXPECT_EQ(strncmp((char*)digest.data,
                      (char*)expected_digest.data(),
                      32),
              0);
}
