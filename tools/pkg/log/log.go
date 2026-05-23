// SPDX-License-Identifier: Apache-2.0
// Package log provides colored terminal output for UmbraVOX CLI tools.
package log

import (
	"fmt"
	"os"
)

// ANSI color codes.
const (
	Red    = "\033[0;31m"
	Green  = "\033[0;32m"
	Blue   = "\033[0;34m"
	Yellow = "\033[1;33m"
	NC     = "\033[0m"
)

// Msg prints a tagged, colored message to stderr.
func Msg(color, tag, msg string) {
	fmt.Fprintf(os.Stderr, "%s[%s]%s %s\n", color, tag, NC, msg)
}

// Info prints a blue informational message.
func Info(tag, msg string) { Msg(Blue, tag, msg) }

// OK prints a green success message.
func OK(tag, msg string) { Msg(Green, tag, msg) }

// Warn prints a yellow warning message.
func Warn(tag, msg string) { Msg(Yellow, tag, msg) }

// Fail prints a red error message.
func Fail(tag, msg string) { Msg(Red, tag, msg) }
