// SPDX-License-Identifier: Apache-2.0
package main

import (
	"errors"
	"os"
	"path/filepath"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/vmctl"
)

// loadVMDef loads a vm-def YAML by name from the vm-defs/ directory rooted at
// repoRoot.  It returns (spec, nil) on success, (nil, nil) when the file does
// not exist (caller should fall back to defaults), or (nil, err) on a parse
// error.
func loadVMDef(repoRoot, name string) (*vmctl.VMSpec, error) {
	path := filepath.Join(repoRoot, "vm-defs", name+".yaml")
	_, spec, err := vmctl.LoadSpec(path)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil, nil // file absent — caller falls back
		}
		return nil, err
	}
	return spec, nil
}
