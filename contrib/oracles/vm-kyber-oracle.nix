# NixOS VM for Kyber768 / ML-KEM-768 reference KAT oracle.
#
# Fetches the UmbraVoxResearch/kyber fork (pq-crystals Kyber reference),
# compiles an inline KAT generator against the ref sources, runs it at boot,
# writes kyber-kat.json to /output, and shuts down.
#
# At runtime the VM has no network access -- every source tree is fetched and
# compiled during the Nix build phase.
#
# NOTE: Placeholder hashes -- this file will not build until they are filled in.
#       Run:
#         nix-prefetch-git --url https://github.com/UmbraVoxResearch/kyber \
#             --rev <commit-or-tag>
#       to obtain the real rev and sha256.
#
# Usage (inside builder VM):
#   ./uv vm kyber generate-kat
{ pkgs ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  # ===========================================================================
  # Source: UmbraVoxResearch/kyber fork (pq-crystals reference)
  # ===========================================================================
  kyberSrc = pkgs.fetchFromGitHub {
    owner  = "UmbraVoxResearch";
    repo   = "kyber";
    # TODO: replace with real rev
    #   nix-prefetch-git --url https://github.com/UmbraVoxResearch/kyber \
    #       --rev <tag-or-commit>
    rev    = "0000000000000000000000000000000000000000";
    # TODO: replace with real hash
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  # ===========================================================================
  # kyber768-oracle binary derivation
  # ===========================================================================
  kyber768Oracle = pkgs.stdenv.mkDerivation {
    pname   = "kyber768-oracle";
    version = "0.0.0-oracle";
    src     = kyberSrc;

    nativeBuildInputs = [ pkgs.gcc pkgs.gnumake pkgs.openssl.dev ];

    buildPhase = ''
      # Write the inline oracle main.
      # Uses the pq-crystals KAT pattern from PQCgenKAT_kem.c:
      #   - entropy_input[j] = j  (0..47)
      #   - randombytes_init(entropy_input, NULL, 256)
      #   - crypto_kem_keypair(pk, sk)     (non-derand; RNG seeded above)
      #   - crypto_kem_enc(ct, ss, pk)
      #   - crypto_kem_dec(ss2, ct, sk)
      #   - verify ss == ss2
      # NOTE: crypto_kem_keypair_derand is not present in all forks.
      #       The non-derand pattern is used here for portability; the RNG
      #       is fully deterministic because randombytes_init seeds the DRBG
      #       with a known value.
      cat > kyber768_oracle_main.c << 'CEOF'
/*
 * Kyber768 KAT oracle.
 * SPDX-License-Identifier: Apache-2.0
 *
 * Follows the pq-crystals KAT pattern from PQCgenKAT_kem.c.
 * The DRBG (AES256-CTR) is seeded deterministically so each
 * run produces the same output.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "api.h"
#include "nistkat/rng.h"

/* Print a byte array as lowercase hex. */
static void print_hex(const unsigned char *buf, size_t len) {
    for (size_t i = 0; i < len; i++)
        printf("%02x", buf[i]);
}

/*
 * Generate N KAT vectors using the pq-crystals deterministic pattern.
 * For vector i:
 *   entropy_input[j] = j  (j in 0..47)
 *   randombytes_init(entropy_input, NULL, 256)
 *   crypto_kem_keypair(pk, sk)
 *   crypto_kem_enc(ct, ss_enc, pk)
 *   crypto_kem_dec(ss_dec, ct, sk)
 *   assert ss_enc == ss_dec
 */
static int generate_kat(int n) {
    unsigned char entropy_input[48];
    unsigned char pk[CRYPTO_PUBLICKEYBYTES];
    unsigned char sk[CRYPTO_SECRETKEYBYTES];
    unsigned char ct[CRYPTO_CIPHERTEXTBYTES];
    unsigned char ss_enc[CRYPTO_BYTES];
    unsigned char ss_dec[CRYPTO_BYTES];

    printf("{\"schema\":\"umbravox-differential-primitive-v1\","
           "\"primitive\":\"ml-kem-768\",\"vectors\":[\n");

    for (int i = 0; i < n; i++) {
        /* Seed the DRBG deterministically (same as PQCgenKAT_kem.c) */
        for (int j = 0; j < 48; j++)
            entropy_input[j] = (unsigned char)j;
        /* Override the first byte to differentiate vectors */
        entropy_input[0] = (unsigned char)i;
        randombytes_init(entropy_input, NULL, 256);

        /* Key generation (uses seeded DRBG internally) */
        if (crypto_kem_keypair(pk, sk) != 0) {
            fprintf(stderr, "kyber768-oracle: keypair failed at i=%d\n", i);
            return 1;
        }

        /* Encapsulation */
        if (crypto_kem_enc(ct, ss_enc, pk) != 0) {
            fprintf(stderr, "kyber768-oracle: enc failed at i=%d\n", i);
            return 1;
        }

        /* Decapsulation */
        if (crypto_kem_dec(ss_dec, ct, sk) != 0) {
            fprintf(stderr, "kyber768-oracle: dec failed at i=%d\n", i);
            return 1;
        }

        /* Verify shared secrets match */
        if (memcmp(ss_enc, ss_dec, CRYPTO_BYTES) != 0) {
            fprintf(stderr, "kyber768-oracle: ss mismatch at i=%d\n", i);
            return 1;
        }

        if (i > 0) printf(",\n");
        printf("{\"id\":\"kyber768-kat-%d\"", i);
        printf(",\"encap_key_hex\":\""); print_hex(pk, CRYPTO_PUBLICKEYBYTES); printf("\"");
        printf(",\"decap_key_hex\":\""); print_hex(sk, CRYPTO_SECRETKEYBYTES); printf("\"");
        printf(",\"ciphertext_hex\":\""); print_hex(ct, CRYPTO_CIPHERTEXTBYTES); printf("\"");
        printf(",\"shared_secret_hex\":\""); print_hex(ss_enc, CRYPTO_BYTES); printf("\"");
        printf("}");
    }

    printf("\n]}\n");
    return 0;
}

int main(int argc, char *argv[]) {
    if (argc >= 3 && strcmp(argv[1], "kat") == 0) {
        int n = atoi(argv[2]);
        if (n <= 0) {
            fprintf(stderr, "kyber768-oracle: N must be > 0\n");
            return 2;
        }
        return generate_kat(n);
    }

    fprintf(stderr, "Usage: kyber768-oracle kat <N>\n");
    fprintf(stderr, "  Generates N KAT vectors and writes JSON to stdout.\n");
    return 2;
}
CEOF

      # Collect all reference C sources
      KYBER_REF="${kyberSrc}/ref"
      KYBER_NISTKAT="${kyberSrc}/ref/nistkat"

      SRCS="kyber768_oracle_main.c \
        $KYBER_REF/kem.c \
        $KYBER_REF/indcpa.c \
        $KYBER_REF/poly.c \
        $KYBER_REF/polyvec.c \
        $KYBER_REF/ntt.c \
        $KYBER_REF/reduce.c \
        $KYBER_REF/cbd.c \
        $KYBER_REF/fips202.c \
        $KYBER_REF/symmetric-shake.c \
        $KYBER_REF/verify.c \
        $KYBER_NISTKAT/rng.c"

      gcc -O2 \
        -DKYBER_K=3 \
        -I"$KYBER_REF" \
        -I"$KYBER_NISTKAT" \
        $SRCS \
        -lcrypto \
        -o kyber768-oracle
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp kyber768-oracle $out/bin/
    '';

    meta.description = "Kyber768 KAT oracle (pq-crystals reference build)";
  };

  # ===========================================================================
  # Packages available inside the VM
  # ===========================================================================
  oraclePkgs = [
    kyber768Oracle
    pkgs.bashInteractive
    pkgs.coreutils
    pkgs.jq
    pkgs.util-linux
  ];

  # ===========================================================================
  # NixOS configuration
  # ===========================================================================
  nixosConfig = { config, lib, modulesPath, pkgs, ... }: {
    imports = [ ./tiers/base.nix ];

    boot.loader.grub.device = "/dev/vda";

    networking.hostName = "umbravox-kyber-oracle";

    # ---- Network deny-all at runtime ----------------------------------------
    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = [];
    networking.firewall.allowedUDPPorts = [];
    networking.firewall.extraCommands = ''
      iptables -P OUTPUT DROP
      iptables -A OUTPUT -o lo -j ACCEPT
    '';

    environment.systemPackages = oraclePkgs;

    # ---- Output volume (host extracts vectors from here) --------------------
    fileSystems."/output" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=128M" "mode=0700" ];
    };

    # ---- Kyber oracle boot service ------------------------------------------
    # Runs the kyber768-oracle, writes KAT vectors to /output, then shuts down.
    systemd.services.kyber-oracle = {
      description = "Kyber768 KAT oracle -- generate test vectors";
      wantedBy = [ "multi-user.target" ];
      after    = [ "local-fs.target" ];
      path     = oraclePkgs;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "kyber-oracle-run" ''
          set -euo pipefail
          export PATH=/run/current-system/sw/bin:$PATH

          echo "=== kyber768 oracle: generating KAT vectors ==="
          kyber768-oracle kat 5 > /output/kyber-kat.json
          echo "=== kyber768 oracle: done -- wrote /output/kyber-kat.json ==="
        '';
        StandardOutput = "journal+console";
        StandardError  = "journal+console";
        TimeoutStartSec = "120";
      };
    };

    # ---- Shutdown after oracle completes ------------------------------------
    systemd.services.kyber-oracle-shutdown = {
      description = "Shutdown after kyber oracle";
      wantedBy = [ "multi-user.target" ];
      after    = [ "kyber-oracle.service" ];
      serviceConfig = {
        Type     = "oneshot";
        ExecStart = "${pkgs.systemd}/bin/systemctl poweroff";
      };
    };
  };

  # ===========================================================================
  # Build the disk image
  # ===========================================================================
  nixos = import (pkgs.path + "/nixos") {
    system = "x86_64-linux";
    configuration = nixosConfig;
  };

  image = import ./make-disk-image.nix {
    inherit pkgs;
    lib   = pkgs.lib;
    config = nixos.config;
    diskSize          = "auto";
    additionalSpace   = "1024M";
    format            = "raw";
    partitionTableType = "legacy";
    copyChannel       = false;
  };

in image
