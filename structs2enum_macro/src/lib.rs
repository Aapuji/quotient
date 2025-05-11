use proc_macro::TokenStream;
use quote::quote;
use syn::{
    braced, parse::{Parse, ParseStream}, parse_macro_input, token::Comma, Attribute, Ident, Item, LifetimeParam, Result, Token, Visibility
};

struct StructEnumInput {
    flat_attrs: Vec<Attribute>,
    flat_vis: Visibility,
    flat_enum_token: Token![enum],
    flat_enum_name: Ident,
    comma: Comma,
    data_attrs: Vec<Attribute>,
    data_vis: Visibility,
    data_enum_token: Token![enum],
    data_enum_name: Ident,
    content: Vec<Item>,
}

impl Parse for StructEnumInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let flat_attrs = input.call(Attribute::parse_outer)?;
        let flat_vis: Visibility = input.parse()?;
        let flat_enum_token: Token![enum] = input.parse()?;
        let flat_enum_name: Ident = input.parse()?;
        let comma: Comma = input.parse()?;
        let data_attrs = input.call(Attribute::parse_outer)?;
        let data_vis: Visibility = input.parse()?;
        let data_enum_token: Token![enum] = input.parse()?;
        let data_enum_name: Ident = input.parse()?;

        let content;
        braced!(content in input);

        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }

        Ok(StructEnumInput {
            flat_attrs,
            flat_vis,
            flat_enum_token,
            flat_enum_name,
            comma,
            data_attrs,
            data_vis,
            data_enum_token,
            data_enum_name,
            content: items,
        })
    }
}


/// Given a series of structs, generate an enum with those struct names as enum variants and an enum with those struct names as variants with the respective struct as data.
/// 
/// ## Example
/// ```
/// define_enum_from_structs! {
///     #[derive(Debug, Clone)]
///     pub enum MyError,
///     #[derive(Debug)] 
///     pub enum MyDiagnostic {
///         /// A test struct
///         #[derive(Debug)]
///         pub struct A {
///             x: i32,
///         }
///
///         /// Another struct
///         #[derive(Debug, Clone)]
///         pub struct B {
///             y: String,
///         }
///     }
/// }
/// ```
/// The above is equivalent to
/// ```
/// /// A test struct
/// #[derive(Debug)]
/// pub struct A {
///     x: i32,
/// }
/// 
/// /// Another struct
/// #[derive(Debug, Clone)]
/// pub struct B {
///     y: String,
/// }
/// 
/// #[derive(Debug, Clone)]
/// pub enum MyError {
///     A,
///     B
/// }
/// 
/// #[derive(Debug)]
/// pub enum MyDiagnostic {
///     A(A),
///     B(B)
/// }
/// ```
#[proc_macro]
pub fn define_enum_from_structs(input: TokenStream) -> TokenStream {
    let StructEnumInput {
        flat_attrs,
        flat_vis,
        flat_enum_token,
        flat_enum_name,
        comma,
        data_attrs,
        data_vis,
        data_enum_token,
        data_enum_name,
        content,
    } = parse_macro_input!(input as StructEnumInput);

    let mut struct_names = Vec::new();
    let mut output_structs = Vec::new();

    for item in content {
        if let Item::Struct(ref item_struct) = item {
            struct_names.push(item_struct.ident.clone());
        }
        output_structs.push(item);
    }

    let enum_variants = struct_names.iter().map(|name| quote! { #name, });
    let enum_variants_with_data = struct_names.iter().map(|name| quote! { #name(#name), });


    let expanded = quote! {
        #(#output_structs)*

        // Flat enum
        #(#flat_attrs)*
        #flat_vis enum #flat_enum_name {
            #(#enum_variants)*
        }

        // Data enum
        #(#data_attrs)*
        #data_vis enum #data_enum_name {
            #(#enum_variants_with_data)*
        }
    };

    TokenStream::from(expanded)
}
