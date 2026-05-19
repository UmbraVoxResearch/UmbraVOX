//! Protocol Oracle CLI — generates protocol traces from libsignal.
//!
//! Built inside the protocol-oracle VM (nix/vm-libsignal-protocol-oracle.nix).
//! Wraps libsignal-protocol to generate X3DH, PQXDH, and Double Ratchet
//! traces with deterministic key material (ChaCha20-based DRBG, seed 42).
//!
//! Usage:
//!   protocol-oracle generate-all --output /output/traces/
//!   protocol-oracle x3dh --seed 42 --messages 10

use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "protocol-oracle")]
#[command(about = "Generate protocol traces from libsignal")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate all protocol traces
    GenerateAll {
        #[arg(long, default_value = "/output/traces")]
        output: PathBuf,
        #[arg(long, default_value = "42")]
        seed: u64,
    },
    /// Generate X3DH key exchange trace
    X3dh {
        #[arg(long, default_value = "42")]
        seed: u64,
    },
    /// Generate Double Ratchet multi-message trace
    DoubleRatchet {
        #[arg(long, default_value = "42")]
        seed: u64,
        #[arg(long, default_value = "20")]
        messages: usize,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::GenerateAll { output, seed } => {
            fs::create_dir_all(&output).expect("Failed to create output directory");
            println!("Generating protocol traces (seed={}) to {:?}...", seed, output);

            // TODO: When libsignal-protocol dependency is enabled:
            // 1. Initialize deterministic RNG from seed
            // 2. Generate identity keys, prekeys
            // 3. Run X3DH exchange, export shared secret + intermediates
            // 4. Initialize Double Ratchet, exchange messages
            // 5. Export per-step state: root key, chain key, message key
            // 6. Write JSON traces

            println!("SCAFFOLD: libsignal-protocol dependency not yet enabled.");
            println!("Enable by uncommenting the dependency in Cargo.toml");
            println!("and building inside the protocol-oracle VM.");
        }
        Commands::X3dh { seed } => {
            println!("X3DH trace (seed={}): SCAFFOLD — not yet implemented", seed);
        }
        Commands::DoubleRatchet { seed, messages } => {
            println!("DoubleRatchet trace (seed={}, {} messages): SCAFFOLD", seed, messages);
        }
    }
}
