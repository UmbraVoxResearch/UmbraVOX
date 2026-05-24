// SPDX-License-Identifier: Apache-2.0
// Command tar2ext4 converts a tar stream to an ext4 disk image.
// Usage: tar cf - /path | tar2ext4 -o output.img
package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ext4"
)

func main() {
	input := flag.String("i", "", "input tar file (default: stdin)")
	output := flag.String("o", "", "output ext4 image (required)")
	flag.Parse()

	if *output == "" {
		fmt.Fprintln(os.Stderr, "Usage: tar2ext4 -o output.img [-i input.tar]")
		os.Exit(1)
	}

	var in *os.File
	if *input != "" {
		f, err := os.Open(*input)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		defer f.Close()
		in = f
	} else {
		in = os.Stdin
	}

	out, err := os.Create(*output)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	defer out.Close()

	if err := ext4.Convert(in, out); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
