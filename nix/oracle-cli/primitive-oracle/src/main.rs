//! Primitive Oracle CLI — generates test vectors from independent crypto libraries.
//!
//! Built inside the primitive-oracle VM (nix/vm-primitive-oracle-suite.nix).
//! Uses RustCrypto crates as the primary oracle for SHA-2, HMAC, HKDF,
//! AES-GCM, ChaCha20-Poly1305, Ed25519, and X25519.
//!
//! Usage:
//!   primitive-oracle generate-all --output /output/vectors/
//!   primitive-oracle compute sha256 --input-hex 616263

use clap::{Parser, Subcommand};
use hex;
use serde::Serialize;
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "primitive-oracle")]
#[command(about = "Generate crypto test vectors from independent libraries")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate all test vectors to a directory
    GenerateAll {
        #[arg(long, default_value = "/output/vectors")]
        output: PathBuf,
    },
    /// Compute a single primitive on hex input
    Compute {
        /// Primitive name: sha256, sha512, hmac-sha256, x25519, ed25519
        primitive: String,
        /// Hex-encoded input
        #[arg(long)]
        input_hex: String,
    },
}

#[derive(Serialize)]
struct VectorFile {
    schema: String,
    primitive: String,
    oracle_class: String,
    oracle: OracleInfo,
    vectors: Vec<serde_json::Value>,
}

#[derive(Serialize)]
struct OracleInfo {
    name: String,
    crate_version: String,
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::GenerateAll { output } => {
            fs::create_dir_all(&output).expect("Failed to create output directory");
            println!("Generating vectors to {:?}...", output);

            // SHA-256 vectors
            generate_sha256_vectors(&output);
            // TODO: SHA-512, HMAC, HKDF, AES-GCM, ChaCha20-Poly1305, Ed25519, X25519

            println!("Vector generation complete.");
        }
        Commands::Compute { primitive, input_hex } => {
            let input = hex::decode(&input_hex).expect("Invalid hex input");
            match primitive.as_str() {
                "sha256" => {
                    use sha2::{Sha256, Digest};
                    let result = Sha256::digest(&input);
                    println!("{}", hex::encode(result));
                }
                _ => {
                    eprintln!("Unknown primitive: {}", primitive);
                    std::process::exit(1);
                }
            }
        }
    }
}

fn generate_sha256_vectors(output: &PathBuf) {
    use sha2::{Sha256, Digest};

    let vectors: Vec<serde_json::Value> = vec![
        serde_json::json!({
            "id": "empty",
            "input_hex": "",
            "output_hex": hex::encode(Sha256::digest(b""))
        }),
        serde_json::json!({
            "id": "abc",
            "input_hex": "616263",
            "output_hex": hex::encode(Sha256::digest(b"abc"))
        }),
    ];

    let file = VectorFile {
        schema: "umbravox-differential-primitive-v1".into(),
        primitive: "sha256".into(),
        oracle_class: "independent-primitive".into(),
        oracle: OracleInfo {
            name: "RustCrypto/sha2".into(),
            crate_version: "0.10".into(),
        },
        vectors,
    };

    let path = output.join("sha256-rustcrypto.json");
    let json = serde_json::to_string_pretty(&file).unwrap();
    fs::write(&path, json).unwrap();
    println!("  wrote {:?}", path);
}
