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

#[proc_macro]
pub fn make_main(_item: TokenStream) -> TokenStream {
    r#"fn main() {
    __aoc2023_solution_1();
    }"#
    .parse()
    .unwrap()
}
