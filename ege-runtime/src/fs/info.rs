use std::ffi::{c_char, c_int};

extern "C" fn bb_fs_file_size(path: *const c_char) -> c_int {
    todo!()
}
