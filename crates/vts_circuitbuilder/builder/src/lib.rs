extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if !(p.path.segments.len() == 1 && p.path.segments[0].ident == wrapper) {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        if attr.path().segments.len() == 1 && attr.path().segments[0].ident == "builder" {
            return Some(attr);
        }
    }
    None
}

fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = f.ident.as_ref().unwrap();
    let attr = builder_of(f)?;
    let mut s = None::<syn::LitStr>;
    attr.parse_nested_meta(|meta| {
        if meta.path.is_ident("each") {
            let value = meta.value().unwrap();
            s = value.parse().ok();
            return Ok(());
        }
        unimplemented!()
    })
    .unwrap();
    let s = s.unwrap();
    let arg = syn::Ident::new(&s.value(), s.span());
    let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
    let method = quote! {
        pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
            self.#name.push(#arg);
            self
        }
    };
    Some((&arg == name, method))
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!()
    };

    let optionals = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_inner_type("Option", ty).is_some() || builder_of(&f).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let set_method = if let Some(inner_ty) = ty_inner_type("Option", ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else if builder_of(&f).is_some() {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        };

        match extend_method(&f) {
            None => set_method.into(),
            Some((true, extend_method)) => extend_method,
            Some((false, extend_method)) => {
                let expr = quote! {
                    #set_method
                    #extend_method
                };
                expr.into()
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_inner_type("Option", ty).is_some() || builder_of(&f).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        if builder_of(f).is_some() {
            quote! {
                #name: Vec::new()
            }
        } else {
            quote! {
                #name: None
            }
        }
    });

    let expanded = quote! {
        pub struct #bident {
            #(#optionals,)*
        }

        impl #bident {
            pub fn new() -> #bident {
                #bident {
                    #(#build_empty,)*
                }
            }

            #(#methods)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_fields,)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #bident {
                #bident::new()
            }
        }
    };

    expanded.into()
}
