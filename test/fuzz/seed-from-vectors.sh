#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# seed-from-vectors.sh -- Convert RFC JSON test vectors to raw binary AFL++ seeds.
#
# Usage: bash test/fuzz/seed-from-vectors.sh
#
# Reads test/vectors/rfc/*.json and creates binary seed files in
# test/fuzz/corpus/<primitive>/ for use with afl-fuzz -i.
#
# Requires: jq, xxd (both available in nix-shell).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
VECTORS_DIR="${SCRIPT_DIR}/../../test/vectors/rfc"
CORPUS_DIR="${SCRIPT_DIR}/corpus"

hex2bin() {
    # Convert hex string to raw bytes via xxd
    local hex="$1"
    if [ -z "$hex" ]; then
        return
    fi
    echo -n "$hex" | xxd -r -p
}

uint16be() {
    # Emit a 2-byte big-endian length
    local val="$1"
    printf "\\x$(printf '%02x' $(( (val >> 8) & 0xff )))\\x$(printf '%02x' $((val & 0xff)))"
}

# -----------------------------------------------------------------------
# AES-256-GCM seeds
# Format: key(32) + nonce(12) + aad_len(2 BE) + aad + ct + tag(16)
# -----------------------------------------------------------------------
generate_gcm_seeds() {
    local json_file="${VECTORS_DIR}/aes256gcm-nist.json"
    local out_dir="${CORPUS_DIR}/gcm"
    mkdir -p "$out_dir"

    if [ ! -f "$json_file" ]; then
        echo "[WARN] ${json_file} not found, skipping GCM seeds."
        return
    fi

    local count
    count=$(jq '.vectors | length' "$json_file")
    for i in $(seq 0 $((count - 1))); do
        local id key_hex iv_hex aad_hex ct_hex tag_hex
        id=$(jq -r ".vectors[$i].id" "$json_file")
        key_hex=$(jq -r ".vectors[$i].key_hex" "$json_file")
        iv_hex=$(jq -r ".vectors[$i].iv_hex" "$json_file")
        aad_hex=$(jq -r ".vectors[$i].aad_hex" "$json_file")
        ct_hex=$(jq -r ".vectors[$i].ciphertext_hex" "$json_file")
        tag_hex=$(jq -r ".vectors[$i].tag_hex" "$json_file")

        local aad_len=$(( ${#aad_hex} / 2 ))

        {
            hex2bin "$key_hex"
            hex2bin "$iv_hex"
            uint16be "$aad_len"
            hex2bin "$aad_hex"
            hex2bin "$ct_hex"
            hex2bin "$tag_hex"
        } > "${out_dir}/${id}.bin"

        echo "[GCM] ${id} -> ${out_dir}/${id}.bin"
    done

    # Also include negative vectors
    local neg_count
    neg_count=$(jq '.negative_vectors | length' "$json_file")
    for i in $(seq 0 $((neg_count - 1))); do
        local id key_hex iv_hex aad_hex ct_hex tag_hex
        id=$(jq -r ".negative_vectors[$i].id" "$json_file")
        key_hex=$(jq -r ".negative_vectors[$i].key_hex" "$json_file")
        iv_hex=$(jq -r ".negative_vectors[$i].iv_hex" "$json_file")
        aad_hex=$(jq -r ".negative_vectors[$i].aad_hex" "$json_file")
        ct_hex=$(jq -r ".negative_vectors[$i].ciphertext_hex" "$json_file")
        tag_hex=$(jq -r ".negative_vectors[$i].tag_hex" "$json_file")

        local aad_len=$(( ${#aad_hex} / 2 ))

        {
            hex2bin "$key_hex"
            hex2bin "$iv_hex"
            uint16be "$aad_len"
            hex2bin "$aad_hex"
            hex2bin "$ct_hex"
            hex2bin "$tag_hex"
        } > "${out_dir}/${id}.bin"

        echo "[GCM] ${id} (negative) -> ${out_dir}/${id}.bin"
    done
}

# -----------------------------------------------------------------------
# Ed25519 seeds
# Format: pubkey(32) + sig(64) + msg
# -----------------------------------------------------------------------
generate_ed25519_seeds() {
    local json_file="${VECTORS_DIR}/ed25519-rfc8032.json"
    local out_dir="${CORPUS_DIR}/ed25519"
    mkdir -p "$out_dir"

    if [ ! -f "$json_file" ]; then
        echo "[WARN] ${json_file} not found, skipping Ed25519 seeds."
        return
    fi

    local count
    count=$(jq '.vectors | length' "$json_file")
    for i in $(seq 0 $((count - 1))); do
        local id pub_hex sig_hex msg_hex
        id=$(jq -r ".vectors[$i].id" "$json_file")
        pub_hex=$(jq -r ".vectors[$i].public_key_hex" "$json_file")
        sig_hex=$(jq -r ".vectors[$i].signature_hex" "$json_file")
        msg_hex=$(jq -r ".vectors[$i].message_hex" "$json_file")

        {
            hex2bin "$pub_hex"
            hex2bin "$sig_hex"
            hex2bin "$msg_hex"
        } > "${out_dir}/${id}.bin"

        echo "[ED25519] ${id} -> ${out_dir}/${id}.bin"
    done
}

# -----------------------------------------------------------------------
# X25519 seeds
# Format: scalar(32) + point(32)
# -----------------------------------------------------------------------
generate_x25519_seeds() {
    local json_file="${VECTORS_DIR}/x25519-rfc7748.json"
    local out_dir="${CORPUS_DIR}/x25519"
    mkdir -p "$out_dir"

    if [ ! -f "$json_file" ]; then
        echo "[WARN] ${json_file} not found, skipping X25519 seeds."
        return
    fi

    local count
    count=$(jq '.vectors | length' "$json_file")
    for i in $(seq 0 $((count - 1))); do
        local id priv_hex pub_hex
        id=$(jq -r ".vectors[$i].id" "$json_file")
        priv_hex=$(jq -r ".vectors[$i].private_hex" "$json_file")
        pub_hex=$(jq -r ".vectors[$i].public_hex" "$json_file")

        {
            hex2bin "$priv_hex"
            hex2bin "$pub_hex"
        } > "${out_dir}/${id}.bin"

        echo "[X25519] ${id} -> ${out_dir}/${id}.bin"
    done
}

# -----------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------
echo "=== Generating AFL++ seed corpus from RFC test vectors ==="
generate_gcm_seeds
generate_ed25519_seeds
generate_x25519_seeds
echo "=== Done. Seeds in ${CORPUS_DIR}/ ==="
