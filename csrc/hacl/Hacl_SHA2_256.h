/* MIT License
 *
 * Copyright (c) 2016-2022 INRIA, CMU and Microsoft Corporation
 * Copyright (c) 2022-2023 HACL* Contributors
 *
 * Hacl_SHA2_256.h — HACL* SHA-256 entry point.
 *
 * Implemented in csrc/hacl/Hacl_SHA2_256.c (M13.15.8).
 * Used by csrc/hacl/bridge_sha256.c.
 */
#pragma once
#include <stdint.h>
#include <stddef.h>

/*
 * Hacl_SHA2_256_hash — compute SHA-256 over input, writing 32 bytes to output.
 *
 *   output    : caller-allocated buffer of at least 32 bytes
 *   input     : message bytes (non-const per HACL* convention)
 *   input_len : byte length of input
 */
void Hacl_SHA2_256_hash(uint8_t *output, uint8_t *input, uint32_t input_len);
