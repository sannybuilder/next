use serde::{de, Deserialize, Deserializer, Serialize};

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Attr {
    #[serde(default)]
    pub is_branch: bool,
    #[serde(default)]
    pub is_condition: bool,
    #[serde(default)]
    pub is_constructor: bool,
    #[serde(default)]
    pub is_destructor: bool,
    #[serde(default)]
    pub is_nop: bool,
    #[serde(default)]
    pub is_overload: bool,
    #[serde(default)]
    pub is_segment: bool,
    #[serde(default)]
    pub is_static: bool,
    #[serde(default)]
    pub is_unsupported: bool,
    #[serde(default)]
    pub is_positional: bool,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Param {
    pub r#name: String,
    pub r#source: Option<String>,
    pub r#type: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Command {
    #[serde(default)]
    pub attrs: Attr,
    pub class: Option<String>,
    #[serde(default, deserialize_with = "convert_to_number")]
    pub id: u16,
    pub member: Option<String>,
    pub name: String,
    pub num_params: i32,
    #[serde(default)]
    pub input: Vec<Param>,
    #[serde(default)]
    pub output: Vec<Param>,
    pub short_desc: Option<String>,
    #[serde(default)]
    pub platforms: Vec<String>,
    #[serde(default)]
    pub versions: Vec<String>,
    pub cc: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Extension {
    pub name: String,
    pub commands: Vec<Command>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Meta {
    pub last_update: u64,
    pub url: String,
    pub version: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ClassMeta {
    pub name: String,
    pub constructable: bool,
    pub extends: Option<String>,
    pub desc: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Library {
    pub meta: Meta,
    pub extensions: Vec<Extension>,
    pub classes: Vec<ClassMeta>,
}

pub fn parse_from(content: &str) -> serde_json::Result<Library> {
    serde_json::from_str::<Library>(content)
}

fn convert_to_number<'de, D>(deserializer: D) -> Result<u16, D::Error>
where
    D: Deserializer<'de>,
{
    String::deserialize(deserializer)
        .and_then(|val| u16::from_str_radix(val.as_str(), 16).map_err(de::Error::custom))
}
