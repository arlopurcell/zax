use assert_cmd::Command;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

macro_rules! zax_test {
    ($name:tt, $code:expr) => {
        #[test]
        fn $name() {
            let path = Path::new("test_data").join(stringify!($name));
            let mut stdout_file =
                File::open(path.join("stdout")).expect("Failed to read stdout file");

            let mut expected_stdout = String::new();
            stdout_file
                .read_to_string(&mut expected_stdout)
                .expect("Failed to read stdout file");

            let mut stderr_file =
                File::open(path.join("stderr")).expect("Failed to read stdout file");
            let mut expected_stderr = String::new();
            stderr_file
                .read_to_string(&mut expected_stderr)
                .expect("Failed to read stderr file");

            Command::cargo_bin(env!("CARGO_PKG_NAME"))
                .unwrap()
                .arg(path.join("input.zax").to_str().unwrap())
                .assert()
                .code($code)
                .stdout(expected_stdout)
                .stderr(expected_stderr);
        }
    };

    ($name:tt) => {
        zax_test!($name, 0);
    };
}

zax_test!(local_scope);
zax_test!(while_loop);
zax_test!(func_call);
zax_test!(func_call_type_param);
zax_test!(func_call_return_type);
zax_test!(top_level_return, 1);
zax_test!(fib);
zax_test!(bad_add, 1);
