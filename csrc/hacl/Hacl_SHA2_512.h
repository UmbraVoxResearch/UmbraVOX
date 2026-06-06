/* MIT License
 *
 * Copyright (c) 2016-2022 INRIA, CMU and Microsoft Corporation
 * Copyright (c) 2022-2023 HACL* Contributors
 *
 * Hacl_SHA2_512.h — HACL* SHA-512 entry point.
 *
 * Implemented in csrc/hacl/Hacl_SHA2_512.c (M13.15.9).
 * Used by csrc/hacl/bridge_sha512.c.
 */
#pragma once
#include <stdint.h>
#include <stddef.h>

/*
 * Hacl_SHA2_512_hash — compute SHA-512 over input, writing 64 bytes to output.
 *
 *   output    : caller-allocated buffer of at least 64 bytes
 *   input     : message bytes (non-const per HACL* convention)
 *   input_len : byte length of input
 */
void Hacl_SHA2_512_hash(uint8_t *output, uint8_t *input, uint32_t input_len);
