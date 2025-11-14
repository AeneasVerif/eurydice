# cmake -B build -G "Ninja Multi-Config"
# cmake --build build
# # For release (benchmarks)
# cmake --build build --config Release

cmake_minimum_required(VERSION 3.10..4.0)

project(libcrux-ml-kem
    VERSION 0.1.0
    LANGUAGES C CXX
)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_OSX_ARCHITECTURES "x86_64")

if(NOT MSVC)
    # TODO: Clean up
    add_compile_options(
        -Wall

        # -Wextra
        # -pedantic
        # -Wconversion
        # -Wsign-conversion
        $<$<CONFIG:DEBUG>:-g>
        $<$<CONFIG:DEBUG>:-Og>
        $<$<CONFIG:RELEASE>:-g>
        $<$<CONFIG:RELEASE>:-O3>
    )
endif(NOT MSVC)

set(CMAKE_COLOR_DIAGNOSTICS "ON")

set(EURYDICE_ROOT "../..")
set(OUT_DIR "../../out/test-libcrux")

# For LSP-based editors
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)
include_directories(
    ${PROJECT_SOURCE_DIR}
    ${PROJECT_SOURCE_DIR}/intrinsics
    ${OUT_DIR}
    ${OUT_DIR}/internal
    ${EURYDICE_ROOT}/include
    ${EURYDICE_ROOT}/karamel/include
)
file(GLOB SOURCES
    ${OUT_DIR}/libcrux_core.c
    ${OUT_DIR}/libcrux_platform.c
    ${OUT_DIR}/libcrux_sha3_portable.c
    ${OUT_DIR}/libcrux_mlkem512.c
    ${OUT_DIR}/libcrux_mlkem768.c
    ${OUT_DIR}/libcrux_mlkem1024.c
    ${OUT_DIR}/libcrux_mlkem_portable.c
    ${OUT_DIR}/libcrux_mlkem512_portable.c
    ${OUT_DIR}/libcrux_mlkem768_portable.c
    ${OUT_DIR}/libcrux_mlkem1024_portable.c
)
file(GLOB SOURCES_vec128
    ${OUT_DIR}/libcrux_mlkem_neon.c
    ${OUT_DIR}/libcrux_mlkem512_neon.c
    ${OUT_DIR}/libcrux_mlkem768_neon.c
    ${OUT_DIR}/libcrux_mlkem1024_neon.c
    ${OUT_DIR}/libcrux_sha3_neon.c
)
file(GLOB SOURCES_vec256
    ${OUT_DIR}/libcrux_mlkem_avx2.c
    ${OUT_DIR}/libcrux_mlkem512_avx2.c
    ${OUT_DIR}/libcrux_mlkem768_avx2.c
    ${OUT_DIR}/libcrux_mlkem1024_avx2.c
    ${OUT_DIR}/libcrux_sha3_avx2.c
)

if(${CMAKE_SYSTEM_NAME} MATCHES Linux)
    add_compile_options(
        -fPIC
    )
endif(${CMAKE_SYSTEM_NAME} MATCHES Linux)

# if(${CMAKE_SYSTEM_NAME} MATCHES Linux AND CMAKE_BUILD_TYPE MATCHES "Release")
#     add_compile_options(
#         -flto
#     )
#     add_link_options(-flto)
# endif(${CMAKE_SYSTEM_NAME} MATCHES Linux AND CMAKE_BUILD_TYPE MATCHES "Release")

add_library(ml_kem SHARED ${SOURCES})
add_library(ml_kem_static STATIC ${SOURCES})

# This is only for local testing and we assume avx2 on x64.
if(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|amd64|AMD64")
    message(STATUS "Detected an x64 architecture")
    add_compile_definitions(LIBCRUX_X64)

    add_library(ml_kem_vec256 OBJECT ${SOURCES_vec256})
    target_sources(ml_kem_static PRIVATE $<TARGET_OBJECTS:ml_kem_vec256>)
    target_sources(ml_kem PRIVATE $<TARGET_OBJECTS:ml_kem_vec256>)

    if(NOT MSVC)
        target_compile_options(ml_kem_vec256 PRIVATE
            -mavx
            -mavx2
        )
    endif(NOT MSVC)
endif()

# This is only for local testing and we assume neon on arm64.
if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64|arm64v8" AND DEFINED ENV{LIBCRUX_NEON})
    message(STATUS "Detected an arm64 architecture")
    add_compile_definitions(LIBCRUX_AARCH64)

    add_library(ml_kem_vec128 OBJECT ${SOURCES_vec128})
    target_sources(ml_kem_static PRIVATE $<TARGET_OBJECTS:ml_kem_vec128>)
    target_sources(ml_kem PRIVATE $<TARGET_OBJECTS:ml_kem_vec128>)
endif()

# --- Tests
if(DEFINED ENV{LIBCRUX_UNPACKED})
    add_compile_definitions(LIBCRUX_UNPACKED)
endif(DEFINED ENV{LIBCRUX_UNPACKED})

# Get gtests
include(FetchContent)
FetchContent_Declare(googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.17.0
)

# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

# Get nlohmann json
FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

add_executable(ml_kem_test
    ${PROJECT_SOURCE_DIR}/tests/mlkem768.cc
)
target_link_libraries(ml_kem_test PRIVATE
    ml_kem_static
    gtest_main
    nlohmann_json::nlohmann_json
)

add_executable(sha3_test
    ${PROJECT_SOURCE_DIR}/tests/sha3.cc
)
target_link_libraries(sha3_test PRIVATE
    ml_kem_static
    gtest_main
    nlohmann_json::nlohmann_json
)

# --- Benchmarks
if(DEFINED ENV{LIBCRUX_BENCHMARKS})
    FetchContent_Declare(benchmark
        GIT_REPOSITORY https://github.com/google/benchmark.git
        GIT_TAG v1.9.2
    )
    FetchContent_MakeAvailable(benchmark)

    add_executable(ml_kem_bench
        ${PROJECT_SOURCE_DIR}/benches/mlkem768.cc
    )
    target_link_libraries(ml_kem_bench PRIVATE
        ml_kem_static
        benchmark::benchmark
    )

    if(DEFINED ENV{SYMCRYPT_PATH})
        message("Symcrypt path: $ENV{SYMCRYPT_PATH}")
        add_compile_definitions(LIBCRUX_SYMCRYPT)
        target_include_directories(ml_kem_bench PRIVATE $ENV{SYMCRYPT_PATH})
        target_link_directories(ml_kem_bench PRIVATE $ENV{SYMCRYPT_PATH}/bin/lib)
        target_link_libraries(ml_kem_bench PRIVATE symcrypt)
    endif(DEFINED ENV{SYMCRYPT_PATH})

    add_executable(ml_kem_keygen
        ${PROJECT_SOURCE_DIR}/benches/mlkem768_keygen.cc
    )
    target_link_libraries(ml_kem_keygen PRIVATE
        ml_kem_static
        benchmark::benchmark
    )

    add_executable(ml_kem_encaps
        ${PROJECT_SOURCE_DIR}/benches/mlkem768_encaps.cc
    )
    target_link_libraries(ml_kem_encaps PRIVATE
        ml_kem_static
        benchmark::benchmark
    )

    if(NOT MSVC)
        # We benchmark internal functions here that are inlined and thus not available
        # in MSVC.
        add_executable(sha3_bench
            ${PROJECT_SOURCE_DIR}/benches/sha3.cc
        )
        target_link_libraries(sha3_bench PRIVATE
            ml_kem_static
            benchmark::benchmark
        )
    endif(NOT MSVC)
endif(DEFINED ENV{LIBCRUX_BENCHMARKS})
