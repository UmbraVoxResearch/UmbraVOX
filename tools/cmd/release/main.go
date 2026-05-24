// Command release packages UmbraVOX release artifacts for supported targets.
//
// Usage: go run ./cmd/release <target>
//
// Targets: source, linux, windows-cli, macos-terminal, bsd-terminal, freedos, all
package main

import (
	"archive/tar"
	"archive/zip"
	"compress/gzip"
	"crypto/sha256"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"time"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, usage)
		os.Exit(1)
	}
	target := os.Args[1]

	root := envOr("UMBRAVOX_ROOT", must(os.Getwd()))
	outDir := filepath.Join(root, "build", "releases")
	version := envOr("UMBRAVOX_RELEASE_VERSION", gitDescribe())
	commit := envOr("UMBRAVOX_RELEASE_COMMIT", gitRevParse())
	stamp := envOr("UMBRAVOX_RELEASE_STAMP", time.Now().UTC().Format("20060102T150405Z"))

	ctx := &releaseCtx{root: root, outDir: outDir, version: version, commit: commit, stamp: stamp}

	ensureCleanTree()
	ensureTaggedRelease()
	must0(os.MkdirAll(outDir, 0o755))

	targets := map[string]func() string{
		"source":         func() string { return ctx.buildSource("source", "source", "tgz", "BUILDING.txt", noteSource) },
		"windows-cli":    func() string { return ctx.buildSource("windows-cli", "windows-cli-source", "zip", "BUILDING-WINDOWS.txt", noteWindows) },
		"macos-terminal": func() string { return ctx.buildSource("macos-terminal", "macos-terminal-source", "tgz", "BUILDING-MACOS.txt", noteMacOS) },
		"bsd-terminal":   func() string { return ctx.buildSource("bsd-terminal", "bsd-terminal-source", "tgz", "BUILDING-BSD.txt", noteBSD) },
		"freedos":        func() string { return ctx.buildSource("freedos", "freedos-source", "zip", "FREEDOS-STATUS.txt", noteFreeDOS) },
		"linux":          func() string { fatalf("linux native binary bundling not yet implemented in Go; use scripts/release-package.sh"); return "" },
		"appimage":       func() string { fatalf("appimage scaffold not yet implemented in Go; use scripts/release-package.sh"); return "" },
	}

	if target == "all" {
		for _, t := range []string{"source", "windows-cli", "macos-terminal", "bsd-terminal", "freedos"} {
			fmt.Println(targets[t]())
		}
		return
	}
	fn, ok := targets[target]
	if !ok {
		fmt.Fprintln(os.Stderr, usage)
		os.Exit(1)
	}
	fmt.Println(fn())
}

type releaseCtx struct {
	root, outDir, version, commit, stamp string
}

func (c *releaseCtx) buildSource(target, suffix, archiveKind, noteFile, noteBody string) string {
	pkg := fmt.Sprintf("umbravox-%s-%s", c.version, suffix)
	stage := filepath.Join(c.outDir, pkg)
	os.RemoveAll(stage)

	stageSourceTree(stage)
	c.writeManifest(stage, target, "source")
	writeFile(filepath.Join(stage, noteFile), noteBody)

	// checksums
	writeContentsSHA256(stage)
	writeScriptSHA256(stage)
	appendToManifest(stage)

	var artifact string
	if archiveKind == "zip" {
		artifact = stage + ".zip"
		createZip(stage, artifact)
	} else {
		artifact = stage + ".tar.gz"
		createTarGz(stage, artifact)
	}
	writeArtifactSHA256(artifact)
	return artifact
}

func (c *releaseCtx) writeManifest(stage, target, kind string) {
	lines := []string{
		"name=UmbraVOX",
		"target=" + target,
		"artifact_kind=" + kind,
		"version=" + c.version,
		"commit=" + c.commit,
		"timestamp_utc=" + c.stamp,
		fmt.Sprintf("builder=%s/%s", runtime.GOOS, runtime.GOARCH),
	}
	writeFile(filepath.Join(stage, "RELEASE-MANIFEST.txt"), strings.Join(lines, "\n")+"\n")
}

// stageSourceTree copies every git-tracked file into stage.
func stageSourceTree(stage string) {
	out := must(exec.Command("git", "ls-files", "-z").Output())
	for _, path := range strings.Split(string(out), "\000") {
		if path == "" {
			continue
		}
		dst := filepath.Join(stage, path)
		must0(os.MkdirAll(filepath.Dir(dst), 0o755))
		copyFile(path, dst)
	}
}

func writeContentsSHA256(stage string) {
	var paths []string
	filepath.WalkDir(stage, func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		rel := must(filepath.Rel(stage, p))
		paths = append(paths, rel)
		return nil
	})
	sort.Strings(paths)
	var sb strings.Builder
	for _, rel := range paths {
		h := fileSHA256(filepath.Join(stage, rel))
		fmt.Fprintf(&sb, "%s  ./%s\n", h, rel)
	}
	writeFile(filepath.Join(stage, "CONTENTS.SHA256"), sb.String())
}

func writeScriptSHA256(stage string) {
	// Hash this Go source as the "release script".
	self := must(os.Executable())
	if _, err := os.Stat(self); err != nil {
		// Fallback: hash the source file relative to cwd.
		self = "tools/cmd/release/main.go"
	}
	h := fileSHA256(self)
	writeFile(filepath.Join(stage, "RELEASE-SCRIPT.SHA256"), fmt.Sprintf("%s  %s\n", h, self))
}

func appendToManifest(stage string) {
	f := must(os.OpenFile(filepath.Join(stage, "RELEASE-MANIFEST.txt"), os.O_APPEND|os.O_WRONLY, 0o644))
	defer f.Close()
	fmt.Fprintln(f, "contents_sha256_file=CONTENTS.SHA256")
	fmt.Fprintln(f, "release_script_sha256_file=RELEASE-SCRIPT.SHA256")
}

func writeArtifactSHA256(artifact string) {
	h := fileSHA256(artifact)
	writeFile(artifact+".sha256", fmt.Sprintf("%s  %s\n", h, filepath.Base(artifact)))
}

// ---------- archive helpers ----------

func createTarGz(stage, artifact string) {
	f := must(os.Create(artifact))
	defer f.Close()
	gw := gzip.NewWriter(f)
	defer gw.Close()
	tw := tar.NewWriter(gw)
	defer tw.Close()
	filepath.WalkDir(stage, func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel := must(filepath.Rel(filepath.Dir(stage), p))
		info := must(d.Info())
		hdr := must(tar.FileInfoHeader(info, ""))
		hdr.Name = rel
		must0(tw.WriteHeader(hdr))
		if !d.IsDir() {
			rf := must(os.Open(p))
			defer rf.Close()
			must(io.Copy(tw, rf))
		}
		return nil
	})
}

func createZip(stage, artifact string) {
	f := must(os.Create(artifact))
	defer f.Close()
	zw := zip.NewWriter(f)
	defer zw.Close()
	filepath.WalkDir(stage, func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		rel := must(filepath.Rel(filepath.Dir(stage), p))
		w := must(zw.Create(rel))
		rf := must(os.Open(p))
		defer rf.Close()
		must(io.Copy(w, rf))
		return nil
	})
}

// ---------- guards ----------

func ensureCleanTree() {
	if envOr("UMBRAVOX_ALLOW_DIRTY_RELEASE", "0") == "1" {
		return
	}
	out, _ := exec.Command("git", "status", "--porcelain").Output()
	if len(strings.TrimSpace(string(out))) > 0 {
		fatalf("refusing release from dirty worktree; commit or stash changes, or set UMBRAVOX_ALLOW_DIRTY_RELEASE=1")
	}
}

func ensureTaggedRelease() {
	if envOr("UMBRAVOX_ALLOW_UNTAGGED_RELEASE", "0") == "1" {
		return
	}
	out, err := exec.Command("git", "describe", "--tags", "--exact-match", "HEAD").Output()
	if err != nil || strings.TrimSpace(string(out)) == "" {
		fatalf("refusing release from untagged commit; tag HEAD or set UMBRAVOX_ALLOW_UNTAGGED_RELEASE=1")
	}
}

// ---------- git helpers ----------

func gitDescribe() string {
	if out, err := exec.Command("git", "describe", "--tags", "--always").Output(); err == nil {
		return strings.TrimSpace(string(out))
	}
	if out, err := exec.Command("git", "rev-parse", "--short", "HEAD").Output(); err == nil {
		return strings.TrimSpace(string(out))
	}
	return "unknown"
}

func gitRevParse() string {
	if out, err := exec.Command("git", "rev-parse", "HEAD").Output(); err == nil {
		return strings.TrimSpace(string(out))
	}
	return "unknown"
}

// ---------- file helpers ----------

func fileSHA256(path string) string {
	f := must(os.Open(path))
	defer f.Close()
	h := sha256.New()
	must(io.Copy(h, f))
	return fmt.Sprintf("%x", h.Sum(nil))
}

func copyFile(src, dst string) {
	sf := must(os.Open(src))
	defer sf.Close()
	info := must(sf.Stat())
	df := must(os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, info.Mode()))
	defer df.Close()
	must(io.Copy(df, sf))
}

func writeFile(path, content string) {
	must0(os.MkdirAll(filepath.Dir(path), 0o755))
	must0(os.WriteFile(path, []byte(content), 0o644))
}

// ---------- utilities ----------

func envOr(key, fallback string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return fallback
}

func fatalf(format string, args ...any) {
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	os.Exit(1)
}

func must[T any](v T, err error) T {
	if err != nil {
		fatalf("%v", err)
	}
	return v
}

func must0(err error) {
	if err != nil {
		fatalf("%v", err)
	}
}

// ---------- target notes ----------

const usage = `usage: release <target>

targets:
  source          generic source archive
  linux           linux native binary bundle (TODO)
  windows-cli     windows source archive (.zip)
  macos-terminal  macOS source archive (.tar.gz)
  bsd-terminal    BSD source archive (.tar.gz)
  freedos         FreeDOS source archive (.zip)
  appimage        AppImage scaffold (TODO)
  all             build all source targets`

const noteSource = `UmbraVOX generic source release

Enter nix-shell on a supported development host and use the documented
build/test/release commands.
`

const noteWindows = `UmbraVOX Windows CLI source release

This target currently emits a source release, not a prebuilt Windows executable.

Recommended native build approach:
1. Install ghcup on Windows.
2. Install GHC and Cabal.
3. Install the required C toolchain/runtime dependencies.
4. Build with cabal on the native host.
`

const noteMacOS = `UmbraVOX macOS terminal source release

This target currently emits a source release, not a cross-built macOS binary.

Recommended native build approach:
1. Install ghcup on macOS.
2. Install GHC and Cabal.
3. Build and run on the native host terminal.
`

const noteBSD = `UmbraVOX BSD terminal source release

This target currently emits a source release, not a cross-built BSD binary.

Recommended native build approach:
1. Install GHC and Cabal through the BSD package system or ghcup.
2. Build on the native BSD host.
`

const noteFreeDOS = `UmbraVOX FreeDOS release note

The current Haskell implementation does not provide a native FreeDOS runtime.
This artifact is intentionally a research/source package with an explicit
unsupported-runtime note, not a claim of executable DOS support.
`
