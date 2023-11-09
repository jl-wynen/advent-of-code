extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

#[proc_macro_attribute]
pub fn task1(attr: TokenStream, item: TokenStream) -> TokenStream {
    let user_fn = parse_macro_input!(item as ItemFn);
    let user_fn_name = user_fn.sig.ident.clone();

    TokenStream::from(quote! {
        #user_fn
        fn __aoc2023_solution_1() {
            use ::aoc2023::fileio;
            let input = fileio::read_input(file!());
            let res = #user_fn_name(input);
            println!("!! Task 1 result: <|{res}|>");
        }
    })
}

#[proc_macro_attribute]
pub fn task2(attr: TokenStream, item: TokenStream) -> TokenStream {
    std::env::set_var("__AOC2023_TASK2", "1");

    let user_fn = parse_macro_input!(item as ItemFn);
    let user_fn_name = user_fn.sig.ident.clone();

    TokenStream::from(quote! {
        #user_fn
        fn __aoc2023_solution_2() {
            use ::aoc2023::fileio;
            let input = fileio::read_input(file!());
            let res = #user_fn_name(input);
            println!("!! Task 2 result: <|{res}|>");
        }
    })
}

#[proc_macro]
pub fn make_main(_item: TokenStream) -> TokenStream {
    let mut code = r#"fn main() {
    __aoc2023_solution_1();"#
        .to_string();

    if std::env::var("__AOC2023_TASK2").is_ok() {
        code += "\n    __aoc2023_solution_2();";
    }
    code += "\n}";

    code.parse().unwrap()
}
