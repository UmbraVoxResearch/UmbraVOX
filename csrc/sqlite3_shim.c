/* SPDX-License-Identifier: Apache-2.0
 *
 * Minimal shim exposing SQLITE_TRANSIENT as a callable symbol.
 *
 * SQLITE_TRANSIENT is a macro expanding to ((sqlite3_destructor_type)-1),
 * which cannot be imported directly via Haskell FFI.  This file provides
 * a global variable whose value is (void*)(-1) so the Haskell FFI can
 * read it via foreign import ccall "&umbravox_sqlite_transient".
 */
#include <sqlite3.h>

/* Expose SQLITE_TRANSIENT as a global void* variable. */
void *const umbravox_sqlite_transient = (void *)SQLITE_TRANSIENT;
