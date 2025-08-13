#![feature(register_tool)]
#![register_tool(charon)]

// STEP 1: Implementation multiplexing. We provide two macros, one for runtime, and one for
// extracting, which generates a semantically equivalent cascade of if-then-elses, knowing that the
// call to `target_arch` will receive special treatment and be turned into an ifdef.
//
// Remember that the easiest way to see the effect of macro expansion is to call
// `rustc --cfg eurydice -Zunpretty=expanded src/main.rs`

#[cfg(not(eurydice))]
#[macro_export]
macro_rules! arch_match {
    ( $( $(|)? $($x:literal)|* => $e:block )|* | _ => $e_default:block ) => {
        {
            $(
                $(
                    #[cfg(target_arch=$x)]
                    if (runtime_support($x)) {
                        $e
                    } else {
                        $e_default
                    }
                )*
            )*
            #[cfg(all($($(not(target_arch=$x)),*),*))]
            $e_default
        }
    };
}

/*
// First attempt: closer to what krml expects to generate the correct #ifdef-based code, but no way
// to inline `target_arch` prior to running Charon (and reduce the match).
mod eurylib {
    #[charon::opaque]
    pub const TARGET_IS_AARCH64: bool = false;
    #[charon::opaque]
    pub const TARGET_IS_X86_64: bool = false;
}

fn target_arch(x: &str) -> bool {
    match x {
      "aarch64" => eurylib::TARGET_IS_AARCH64,
      "x86_64" => eurylib::TARGET_IS_X86_64,
      _ => panic!("oh noes")
    }
}
*/

// Second attempt: with dedicated support in Eurydice
mod eurylib {
    #[charon::opaque]
    pub fn target_arch(x: &str) -> bool {
        true
    }
}

use eurylib::*;

fn runtime_support(x: &str) -> bool {
    // CPUID, etc.
    true
}

#[cfg(eurydice)]
#[macro_export]
macro_rules! arch_match {
    ( $( $(|)? $($x:literal)|* => $e:block )|* | _ => $e_default:block ) => {
        {
            $(
                $(
                    if (target_arch($x) && runtime_support($x)) {
                        $e
                    } else 
                )*
            )*
            $e_default
        }
    };
}

// STEP 2: stub out intrinsics so that the Vec128 and both Intel and ARM intrinsics can live in the
// same scope, for the purposes of extraction.

#[cfg(not(eurydice))]
mod intrinsics {
    // Intel
    #[cfg(target_arch = "x86")]
    pub use std::arch::x86::*;
    #[cfg(target_arch = "x86_64")]
    pub use std::arch::x86_64::*;
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    pub type Vec128 = __m128i;

    // ARM
    #[cfg(target_arch = "aarch64")]
    pub use core::arch::aarch64::*;
    #[cfg(target_arch = "aarch64")]
    pub type Vec128 = uint16x8_t;
}

#[cfg(eurydice)]
mod intrinsics {
    mod private {
        #[charon::opaque]
        pub struct Vec128 { dummy: () }
    }
    // FIXME: why go through that indirection?
    pub use self::private::Vec128;
    unsafe extern "C" {
        pub fn vdupq_n_u16(x: u16) -> Vec128;
        pub fn vst1q_u16(x: *mut u16, y: Vec128);

        pub fn _mm_set1_epi16(x: i16) -> Vec128;
        pub fn _mm_storeu_si128(x: *mut Vec128, y: Vec128);
    }
}

// STEP 3: client implements their library of optimized implementations for both targets

// TODO: extract into separate file (compiled with -msse2)
#[cfg(any(target_arch = "x86", target_arch = "x86_64", eurydice))]
mod vectorized_intel {
    use super::intrinsics::*;

    pub fn vec128_set_u16(val: u16) -> Vec128 {
        unsafe { _mm_set1_epi16(val as i16) }
    }

    pub fn vec128_store(elem: &mut [u16; 8], val: Vec128) {
        unsafe {
            let addr = elem.as_mut_ptr();
            _mm_storeu_si128(addr as *mut Vec128, val);
        }
    }
}

// TODO: extract into separate file (arm64)
#[cfg(any(target_arch = "aarch64", eurydice))]
mod vectorized_arm {
    use super::intrinsics::*;

    pub fn vec128_set_u16(val: u16) -> Vec128 {
        unsafe { vdupq_n_u16(val) }
    }

    pub fn vec128_store(elem: &mut [u16; 8], val: Vec128) {
        unsafe {
            let addr = elem.as_mut_ptr();
            vst1q_u16(addr, val);
        }
    }

}

// TODO: autogenerate
#[cfg(eurydice)]
mod vectorized_stubs {
    use super::*;
    use super::intrinsics::Vec128;

    pub fn vec128_set_u16(val: u16) -> Vec128 {
        arch_match!{
            | "x86" | "x86_64" => { vectorized_intel::vec128_set_u16(val) }
            | "aarch64" => { vectorized_arm::vec128_set_u16(val) }
            | _ => { panic!("no default impl") }
        }
    }

    pub fn vec128_store(elem: &mut [u16; 8], val: Vec128) {
        arch_match!{
            | "x86" | "x86_64" => { vectorized_intel::vec128_store(elem, val) }
            | "aarch64" => { vectorized_arm::vec128_store(elem, val) }
            | _ => { panic!("no default impl") }
        }
    }
}

// STEP 4: regular build picks either one of these, C extraction multiplexes

#[cfg(all(any(target_arch = "x86", target_arch = "x86_64"), not(eurydice)))]
use vectorized_intel::*;

#[cfg(all(target_arch = "aarch64", not(eurydice)))]
use vectorized_arm::*;

#[cfg(eurydice)]
use vectorized_stubs::*;

// STEP 5: client multiplexes via the macro

fn default_impl() { println!("ok") }

// TODO: extract into separate file (arm64 or sse2)
mod vectorized {
    use super::*;
    pub fn vector_impl() {
        let expected = [1u16; 8];
        let v = vec128_set_u16(1);
        let mut actual = [0u16; 8];
        vec128_store(&mut actual, v);
        assert_eq!(actual, expected);
    }
}

use vectorized::*;

fn main() {
    arch_match!{
        | "x86" | "x86_64" => { vector_impl() }
        | "aarch64" => { vector_impl() }
        | _ => { default_impl() }
    }
    println!("Hello, world!");
}
