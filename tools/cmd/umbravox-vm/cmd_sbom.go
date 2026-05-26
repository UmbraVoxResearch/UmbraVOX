// SPDX-License-Identifier: Apache-2.0
package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
)

// CycloneDX 1.5 JSON structures — hand-built, no external library.

type cdxBOM struct {
	BOMFormat   string       `json:"bomFormat"`
	SpecVersion string       `json:"specVersion"`
	Version     int          `json:"version"`
	Metadata    cdxMetadata  `json:"metadata"`
	Components  []cdxComp    `json:"components"`
}

type cdxMetadata struct {
	Timestamp string     `json:"timestamp"`
	Tools     []cdxTool  `json:"tools"`
	Component cdxMetaComp `json:"component"`
}

type cdxTool struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

type cdxMetaComp struct {
	Type    string `json:"type"`
	Name    string `json:"name"`
	Version string `json:"version"`
}

type cdxComp struct {
	Type     string       `json:"type"`
	Name     string       `json:"name"`
	Version  string       `json:"version"`
	Licenses []cdxLicWrap `json:"licenses,omitempty"`
	PURL     string       `json:"purl,omitempty"`
}

type cdxLicWrap struct {
	License cdxLicense `json:"license"`
}

type cdxLicense struct {
	ID string `json:"id"`
}

func runSBOM(_ []string) int {
	root, err := repo.Root()
	if err != nil {
		log.Fail("sbom", fmt.Sprintf("find repo root: %v", err))
		return 1
	}

	var components []cdxComp
	licenses := map[string]bool{}

	// --- 1. Haskell deps from cabal.project.freeze ---
	hsComps := scanCabalFreeze(root)
	components = append(components, hsComps...)
	for _, c := range hsComps {
		for _, lw := range c.Licenses {
			licenses[lw.License.ID] = true
		}
	}

	// --- 2. CryptoGen components from csrc/generated/*.c ---
	genComps := scanGeneratedC(root)
	components = append(components, genComps...)
	for _, c := range genComps {
		for _, lw := range c.Licenses {
			licenses[lw.License.ID] = true
		}
	}

	// --- 3. Vendored HACL* ---
	if haclComps := scanHACL(root); len(haclComps) > 0 {
		components = append(components, haclComps...)
		for _, c := range haclComps {
			for _, lw := range c.Licenses {
				licenses[lw.License.ID] = true
			}
		}
	}

	// --- 4. Vendored fiat-crypto ---
	if fiatComps := scanFiat(root); len(fiatComps) > 0 {
		components = append(components, fiatComps...)
		for _, c := range fiatComps {
			for _, lw := range c.Licenses {
				licenses[lw.License.ID] = true
			}
		}
	}

	// --- 5. Go deps from tools/go.mod ---
	goComps := scanGoMod(root)
	components = append(components, goComps...)
	for _, c := range goComps {
		for _, lw := range c.Licenses {
			licenses[lw.License.ID] = true
		}
	}

	// --- 6. Read VERSION ---
	version := readVersion(root)

	// --- Build CycloneDX 1.5 ---
	bom := cdxBOM{
		BOMFormat:   "CycloneDX",
		SpecVersion: "1.5",
		Version:     1,
		Metadata: cdxMetadata{
			Timestamp: time.Now().UTC().Format(time.RFC3339),
			Tools: []cdxTool{
				{Name: "umbravox-vm", Version: "1.0.0"},
			},
			Component: cdxMetaComp{
				Type:    "application",
				Name:    "UmbraVOX",
				Version: version,
			},
		},
		Components: components,
	}

	data, err := json.MarshalIndent(bom, "", "  ")
	if err != nil {
		log.Fail("sbom", fmt.Sprintf("marshal JSON: %v", err))
		return 1
	}

	outDir := filepath.Join(root, "build")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		log.Fail("sbom", fmt.Sprintf("create build dir: %v", err))
		return 1
	}

	outPath := filepath.Join(outDir, "sbom.cdx.json")
	if err := os.WriteFile(outPath, append(data, '\n'), 0o644); err != nil {
		log.Fail("sbom", fmt.Sprintf("write %s: %v", outPath, err))
		return 1
	}

	log.OK("sbom", fmt.Sprintf("SBOM generated: %d components, %d licenses", len(components), len(licenses)))
	log.Info("sbom", outPath)
	return 0
}

// scanCabalFreeze parses cabal.project.freeze for "any.<pkg> ==<version>" lines.
func scanCabalFreeze(root string) []cdxComp {
	path := filepath.Join(root, "cabal.project.freeze")
	f, err := os.Open(path)
	if err != nil {
		log.Warn("sbom", fmt.Sprintf("cabal.project.freeze: %v", err))
		return nil
	}
	defer f.Close()

	re := regexp.MustCompile(`any\.(\S+)\s+==(\S+),?`)
	var comps []cdxComp
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		m := re.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		name := m[1]
		ver := m[2]
		// Strip trailing comma from version if present.
		ver = strings.TrimRight(ver, ",")
		comps = append(comps, cdxComp{
			Type:    "library",
			Name:    name,
			Version: ver,
			PURL:    fmt.Sprintf("pkg:hackage/%s@%s", name, ver),
		})
	}
	return comps
}

// scanGeneratedC discovers CryptoGen-generated C source files.
func scanGeneratedC(root string) []cdxComp {
	dir := filepath.Join(root, "csrc", "generated")
	entries, err := os.ReadDir(dir)
	if err != nil {
		log.Warn("sbom", fmt.Sprintf("csrc/generated: %v", err))
		return nil
	}
	var comps []cdxComp
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".c") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".c")
		comps = append(comps, cdxComp{
			Type:    "library",
			Name:    "cryptogen-" + name,
			Version: "0.0.0",
			Licenses: []cdxLicWrap{
				{License: cdxLicense{ID: "Apache-2.0"}},
			},
		})
	}
	return comps
}

// scanHACL checks for vendored HACL* sources.
func scanHACL(root string) []cdxComp {
	dir := filepath.Join(root, "csrc", "hacl")
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var cFiles []string
	for _, e := range entries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".c") {
			cFiles = append(cFiles, strings.TrimSuffix(e.Name(), ".c"))
		}
	}
	if len(cFiles) == 0 {
		return nil
	}
	var comps []cdxComp
	for _, name := range cFiles {
		comps = append(comps, cdxComp{
			Type:    "library",
			Name:    "hacl-" + name,
			Version: "0.0.0",
			Licenses: []cdxLicWrap{
				{License: cdxLicense{ID: "Apache-2.0"}},
			},
			PURL: "pkg:github/cryspen/hacl-packages",
		})
	}
	return comps
}

// scanFiat checks for vendored fiat-crypto sources.
func scanFiat(root string) []cdxComp {
	dir := filepath.Join(root, "csrc", "fiat")
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var cFiles []string
	for _, e := range entries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".c") {
			cFiles = append(cFiles, strings.TrimSuffix(e.Name(), ".c"))
		}
	}
	if len(cFiles) == 0 {
		return nil
	}
	var comps []cdxComp
	for _, name := range cFiles {
		comps = append(comps, cdxComp{
			Type:    "library",
			Name:    "fiat-" + name,
			Version: "0.0.0",
			Licenses: []cdxLicWrap{
				{License: cdxLicense{ID: "MIT"}},
			},
			PURL: "pkg:github/mit-plv/fiat-crypto",
		})
	}
	return comps
}

// scanGoMod parses tools/go.mod for require directives.
func scanGoMod(root string) []cdxComp {
	path := filepath.Join(root, "tools", "go.mod")
	f, err := os.Open(path)
	if err != nil {
		log.Warn("sbom", fmt.Sprintf("tools/go.mod: %v", err))
		return nil
	}
	defer f.Close()

	var comps []cdxComp
	scanner := bufio.NewScanner(f)
	inRequire := false
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())

		// Handle require block
		if line == "require (" {
			inRequire = true
			continue
		}
		if inRequire && line == ")" {
			inRequire = false
			continue
		}

		// Single-line require
		if strings.HasPrefix(line, "require ") && !strings.Contains(line, "(") {
			parts := strings.Fields(line)
			if len(parts) >= 3 {
				comps = append(comps, goModComp(parts[1], parts[2]))
			}
			continue
		}

		// Inside require block
		if inRequire {
			parts := strings.Fields(line)
			if len(parts) >= 2 {
				comps = append(comps, goModComp(parts[0], parts[1]))
			}
		}
	}
	return comps
}

func goModComp(mod, ver string) cdxComp {
	return cdxComp{
		Type:    "library",
		Name:    mod,
		Version: ver,
		PURL:    fmt.Sprintf("pkg:golang/%s@%s", mod, ver),
	}
}

// readVersion extracts the version string from src/UmbraVox/Version.hs.
func readVersion(root string) string {
	path := filepath.Join(root, "src", "UmbraVox", "Version.hs")
	data, err := os.ReadFile(path)
	if err != nil {
		log.Warn("sbom", fmt.Sprintf("Version.hs: %v", err))
		return "unknown"
	}
	re := regexp.MustCompile(`version\s*=\s*"([^"]+)"`)
	m := re.FindSubmatch(data)
	if m == nil {
		return "unknown"
	}
	return string(m[1])
}
