use proc_macro;

#[proc_macro]
pub fn binary_cstr(input: proc_macro::TokenStream) -> proc_macro::TokenStream
{
    // "my_variable\0".as_ptr() as *const _

    let input_str = syn::parse_macro_input!(input as syn::LitStr);
    let input_value = input_str.value();

    let cstr = format!("{}\0", input_value);

    let output = quote::quote! {
        {
            const CSTR: &'static [u8] = #cstr.as_bytes();
            CSTR.as_ptr() as *const i8
        }
    };

    output.into()
}
