// raw block extensions
// 1. https://github.com/typst/typst/blob/37249c20f793689b523cbb379f30b37466ca51b9/crates/typst/src/text/raw.rs#L744
// 2. https://github.com/typst/typst/blob/37249c20f793689b523cbb379f30b37466ca51b9/crates/typst/src/text/raw.rs#L273
// 3. https://github.com/typst/typst/blob/37249c20f793689b523cbb379f30b37466ca51b9/crates/typst/src/text/raw.rs#L349
//     -> https://docs.rs/syntect/latest/src/syntect/parsing/syntax_set.rs.html#193-201

use std::{
    collections::{BTreeSet, HashMap, HashSet},
    hash::Hash, iter::MapWhile,
};
use indexmap::IndexMap;

fn main() {
    let syn_set = two_face::syntax::extra_no_newlines();

    let tag_remap = get_tag_remap();
    let identifier_remap = get_identifier_remap();

    let mut langs: BTreeSet<String> = BTreeSet::new();
    // note: lower cace "F" and "f" -> "f" and "f", so use HashSet
    let mut lang_tags_map: IndexMap<String, HashSet<String>> = IndexMap::new();

    for syntax in syn_set.syntaxes() {
        // println!("{} {:?}", syntax.name, syntax.file_extensions);
        let whitespace_in_lang_name = syntax.name.find(char::is_whitespace).is_some();
        let lang_name = syntax
            .name
            .split_whitespace()
            .next()
            .unwrap()
            .to_ascii_lowercase();

        let lang_identifier = if whitespace_in_lang_name || lang_name.contains('#') {
            // # is not valid in Emacs lisp print expression
            let tag0 = syntax.file_extensions.first();
            if tag0.is_none() {
                continue;
            }
            tag0.unwrap().to_ascii_lowercase()
        } else {
            lang_name.clone()
        };

        let remapped_identifier = identifier_remap
            .get(&lang_identifier)
            .map_or(lang_identifier, |id| id.clone());

        langs.insert(remapped_identifier.clone());

        let mut lang_name_in_tags = false;
        let mut tags: HashSet<String> = HashSet::new();
        for tag in syntax.file_extensions.iter() {
            let tag = tag.to_ascii_lowercase();
            if tag == lang_name {
                lang_name_in_tags = true;
            }
            if let Some(new_identifier) = tag_remap.get(&tag) {
                langs.insert(new_identifier.clone());
                if let Some(tags) = lang_tags_map.get_mut(new_identifier) {
                    (*tags).insert(tag);
                    continue;
                }
                lang_tags_map.insert(new_identifier.clone(), HashSet::from([tag]));
                continue;
            }
            tags.insert(tag);
        }
        if !(whitespace_in_lang_name || lang_name_in_tags) {
            tags.insert(lang_name);
        }
        if let Some(temp_tags) = lang_tags_map.get_mut(&remapped_identifier) {
            temp_tags.extend(tags);
        } else {
            lang_tags_map.insert(remapped_identifier.clone(), tags);
        }
    }
    langs.insert("typst".to_string());
    lang_tags_map.insert(
        "typst".into(),
        HashSet::from(["typ".into(), "typc".into(), "typst".into()]),
    );

    remove_duplicate_tags(&mut lang_tags_map);

    println!("---------- lang_identifiers --------------");
    println!("{:?}", langs);

    println!("---------- lang_tags_hash_table --------------");
    println!("{}", format_lang_tags_hash_table(&lang_tags_map));

    println!("---------- tag_lang_hash_table --------------");
    println!("{}", format_tag_lang_hash_table(&lang_tags_map));
}

fn remove_duplicate_tags(lang_tags_map: &mut IndexMap<String, HashSet<String>>) {
    // according to https://docs.rs/syntect/latest/src/syntect/parsing/syntax_set.rs.html#185
    // only keeps the last unique tag
    let mut all_tags:HashSet<String> = HashSet::new();
    for tags in lang_tags_map.values_mut().rev() {
        tags.retain(|tag| {
            if all_tags.contains(tag) {
                false
            } else {
                all_tags.insert(tag.clone());
                true
            }
        });
    }
}

fn format_hash_table(total: u32, data_str: &String) -> String {
    format!(
        "#s(hash-table
size {}
test equal
data
(
{}
))",
        total, data_str
    )
}

fn format_lang_tags_hash_table(lang_tags_map: &IndexMap<String, HashSet<String>>) -> String {
    let mut total: u32 = 0;
    let mut data_str = String::new();
    for (lang, tags) in lang_tags_map.iter() {
        data_str += format!("{lang} (").as_str();
        for tag in tags {
            data_str += format!("\"{tag}\" ").as_str();
        }
        data_str.push_str(")\n");
        total += 1;
    }
    format_hash_table(total + 10, &data_str)
}

fn format_tag_lang_hash_table(lang_tags_map: &IndexMap<String, HashSet<String>>) -> String {
    const LINE_MAX_ENTRY_NUM: u32 = 3;
    let mut total: u32 = 0;
    let mut i: u32 = 0;
    let mut data_str = String::new();
    for (lang, tags) in lang_tags_map.iter() {
        for tag in tags {
            if i == 0 {
                data_str.push(' ');
            }
            data_str.push_str(format!(" \"{}\" {}", tag, lang).as_str());
            if i == LINE_MAX_ENTRY_NUM - 1 {
                data_str.push('\n');
            }
            i = (i + 1) % LINE_MAX_ENTRY_NUM;
            total += 1;
        }
    }

    format_hash_table(total + 10, &data_str)
}

// All tree sitter major modes <<<
// use `typst-ts/util/els/get-all-ts-major-modes' function in `side/utils.el' file
// ;; "(awk-ts-mode bash-ts-mode bibtex-ts-mode c-ts-mode csharp-ts-mode clojure-ts-mode cmake-ts-mode commonlisp-ts-mode c++-ts-mode css-ts-mode dart-ts-mode dockerfile-ts-mode elixir-ts-mode go-ts-mode go-mod-ts-mode heex-ts-mode html-ts-mode java-ts-mode js-ts-mode json-ts-mode julia-ts-mode kotlin-ts-mode latex-ts-mode lua-ts-mode makefile-ts-mode markdown-ts-mode protobuf-ts-mode python-ts-mode r-ts-mode ruby-ts-mode rust-ts-mode toml-ts-mode tsx-ts-mode typescript-ts-mode typst-ts-mode verilog-ts-mode vhdl-ts-mode yaml-ts-mode ert-results-mode gdb-breakpoints-mode dape-info-breakpoints-mode mermaid-ts-mode)"

fn slice_to_hashmap<M, N, K, V>(array: &[(K, V)]) -> HashMap<M, N>
where
    K: Hash + Eq + Clone,
    V: Clone,
    M: Hash + Eq + From<K>,
    N: Clone + From<V>,
{
    let mut map: HashMap<M, N> = HashMap::new();
    for (ref key, ref value) in array {
        map.insert(key.clone().into(), value.clone().into());
    }
    map
}

fn get_tag_remap() -> HashMap<String, String> {
    let map = &[("clisp", "commonlisp")];
    slice_to_hashmap(map)
}

fn get_identifier_remap() -> HashMap<String, String> {
    let map = &[("sh", "bash"), ("cs", "c-sharp"), ("c++", "cpp")];
    slice_to_hashmap(map)
}
