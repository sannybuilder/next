# version=$(grep -oP 'version\s*=\s*"\K[0-9]+\.[0-9]+\.[0-9]+' somersault-wasm/Cargo.toml)
# version_with_underscores=$(echo "$version" | tr '.' '_')
# echo "Building ssc_$version_with_underscores.wasm file"

wasm-pack build somersault-wasm --target web --out-name "ssc_dev"
