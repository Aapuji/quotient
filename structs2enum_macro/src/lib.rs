use proc_macro::TokenStream;
use quote::quote;
use syn::{
    braced, parse::{Parse, ParseStream}, parse_macro_input, Attribute, Ident, Item, LifetimeParam, Result, Token, Visibility
};

struct StructEnumInput {
    attrs: Vec<Attribute>,
    vis: Visibility,
    enum_token: Token![enum],
    enum_name: Ident,
    content: Vec<Item>,
}

impl Parse for StructEnumInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let enum_token: Token![enum] = input.parse()?;
        let enum_name: Ident = input.parse()?;

        let content;
        braced!(content in input);

        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }

        Ok(StructEnumInput {
            attrs,
            vis,
            enum_token,
            enum_name,
            content: items,
        })
    }
}


/// Given a series of structs, generate an enum with those struct names as enum variants.
/// 
/// ## Example
/// ```
/// define_enum_from_structs! {
///     #[derive(Debug)]
///     enum MyEnum {
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
#[proc_macro]
pub fn define_enum_from_structs(input: TokenStream) -> TokenStream {
    let StructEnumInput {
        attrs,
        vis,
        enum_token,
        enum_name,
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

    let expanded = quote! {
        #(#output_structs)*

        #(#attrs)*
        #vis enum #enum_name {
            #(#enum_variants)*
        }
    };

    TokenStream::from(expanded)
}
