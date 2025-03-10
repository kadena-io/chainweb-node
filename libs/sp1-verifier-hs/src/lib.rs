use std::ffi::CStr;
use std::os::raw::c_char;
use sp1_verifier::{PlonkError, PlonkVerifier};

#[unsafe(no_mangle)]
pub extern "C" fn verify_proof_ffi(
    proof_ptr: *const u8,
    proof_len: usize,

    sp1_public_inputs_ptr: *const u8,
    sp1_public_inputs_len: usize,

    sp1_vkey_hash_ptr: *const c_char,

    plonk_vk_ptr: *const u8,
    plonk_vk_len: usize,
) -> i32 {
    let proof = unsafe { std::slice::from_raw_parts(proof_ptr, proof_len) };
    let sp1_public_inputs = unsafe { std::slice::from_raw_parts(sp1_public_inputs_ptr, sp1_public_inputs_len) };

    let sp1_vkey_hash_cstr = unsafe { CStr::from_ptr(sp1_vkey_hash_ptr) };
    let sp1_vkey_hash = match sp1_vkey_hash_cstr.to_str() {
        Ok(s) => s,
        Err(_) => return -100
    };

    let plonk_vk: &[u8] = unsafe { std::slice::from_raw_parts(plonk_vk_ptr, plonk_vk_len) };

    match PlonkVerifier::verify(proof, sp1_public_inputs, sp1_vkey_hash, plonk_vk) {
        Ok(_) => 0,
        Err(e) => match e {
            PlonkError::BeyondTheModulus => -1,
            PlonkError::Bsb22CommitmentMismatch => -2,
            PlonkError::ChallengeAlreadyComputed => -3,
            PlonkError::ChallengeNotFound => -4,
            PlonkError::DSTTooLarge => -5,
            PlonkError::EllTooLarge => -6,
            PlonkError::InverseNotFound => -7,
            PlonkError::InvalidNumberOfDigests => -8,
            PlonkError::InvalidWitness => -9,
            PlonkError::PairingCheckFailed => -10,
            PlonkError::PreviousChallengeNotComputed => -11,
            PlonkError::TranscriptError => -12,
            PlonkError::PlonkVkeyHashMismatch => -13,
            PlonkError::GeneralError(_) => -99,
        }
    }
}
