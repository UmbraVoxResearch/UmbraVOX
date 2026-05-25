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

	abiArch := envOr("UMBRAVOX_RELEASE_ABI_ARCH", "x86_64")
	abiKernelMin := envOr("UMBRAVOX_RELEASE_ABI_KERNEL_MIN", "3.10")
	abiGlibcMin := envOr("UMBRAVOX_RELEASE_ABI_GLIBC_MIN", "2.31")

	ctx := &releaseCtx{
		root: root, outDir: outDir, version: version, commit: commit, stamp: stamp,
		abiArch: abiArch, abiKernelMin: abiKernelMin, abiGlibcMin: abiGlibcMin,
	}

	ensureCleanTree()
	ensureTaggedRelease()
	must0(os.MkdirAll(outDir, 0o755))

	targets := map[string]func() string{
		"source":         func() string { return ctx.buildSource("source", "source", "tgz", "BUILDING.txt", noteSource) },
		"windows-cli":    func() string { return ctx.buildSource("windows-cli", "windows-cli-source", "zip", "BUILDING-WINDOWS.txt", noteWindows) },
		"macos-terminal": func() string { return ctx.buildSource("macos-terminal", "macos-terminal-source", "tgz", "BUILDING-MACOS.txt", noteMacOS) },
		"bsd-terminal":   func() string { return ctx.buildSource("bsd-terminal", "bsd-terminal-source", "tgz", "BUILDING-BSD.txt", noteBSD) },
		"freedos":        func() string { return ctx.buildSource("freedos", "freedos-source", "zip", "FREEDOS-STATUS.txt", noteFreeDOS) },
		"linux":          func() string { return ctx.buildLinux() },
		"appimage":       func() string { return ctx.buildAppImage() },
		"freebsd":        func() string { return ctx.buildPlatform("freebsd") },
		"openbsd":        func() string { return ctx.buildPlatform("openbsd") },
		"netbsd":         func() string { return ctx.buildPlatform("netbsd") },
		"illumos":        func() string { return ctx.buildPlatform("illumos") },
		"linux-arm64":    func() string { return ctx.buildPlatform("linux-arm64") },
	}

	if target == "all" {
		for _, t := range []string{"source", "windows-cli", "macos-terminal", "bsd-terminal", "freedos", "linux", "appimage", "freebsd", "openbsd", "netbsd", "illumos", "linux-arm64"} {
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
	abiArch, abiKernelMin, abiGlibcMin   string
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

// buildLinux creates a self-contained Linux binary bundle with bundled
// dynamic loader and shared libraries, matching scripts/release-package.sh.
func (c *releaseCtx) buildLinux() string {
	requireCmd("cabal")
	requireCmd("patchelf")
	requireCmd("file")
	requireCmd("ldd")

	pkg := fmt.Sprintf("umbravox-%s-linux-x86_64", c.version)
	stage := filepath.Join(c.outDir, pkg)
	artifact := stage + ".tar.gz"

	os.RemoveAll(stage)
	must0(os.MkdirAll(filepath.Join(stage, "bin"), 0o755))
	must0(os.MkdirAll(filepath.Join(stage, "lib"), 0o755))

	copyCommonDocs(c.root, stage)
	c.writeManifestLinux(stage, "linux-x86_64", "native-binary")
	writeFile(filepath.Join(stage, "PORTABILITY.txt"), noteLinux)

	// Build and locate the binary.
	cmd := exec.Command("cabal", "build", "exe:umbravox")
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	must0(cmd.Run())
	binPath := strings.TrimSpace(string(must(exec.Command("cabal", "list-bin", "exe:umbravox").Output())))

	wrappedBin := filepath.Join(stage, "bin", "umbravox.bin")
	copyFile(binPath, wrappedBin)

	// Get the ELF interpreter.
	interp := strings.TrimSpace(string(must(exec.Command("patchelf", "--print-interpreter", binPath).Output())))
	interpBase := filepath.Base(interp)

	// Collect direct dependencies: interpreter + ldd output.
	deps := make(map[string]bool)
	deps[interp] = true
	for _, dep := range lddAbsolutePaths(binPath) {
		deps[dep] = true
	}

	// Copy direct deps into lib/.
	for dep := range deps {
		copyDep(stage, dep)
	}

	// Expand transitive ELF closure.
	changed := true
	for changed {
		changed = false
		entries := must(os.ReadDir(filepath.Join(stage, "lib")))
		for _, e := range entries {
			if e.IsDir() {
				continue
			}
			libPath := filepath.Join(stage, "lib", e.Name())
			for _, tdep := range lddAbsolutePaths(libPath) {
				tbase := filepath.Base(tdep)
				if _, err := os.Stat(filepath.Join(stage, "lib", tbase)); err != nil {
					copyDep(stage, tdep)
					changed = true
				}
			}
		}
	}

	// Write the wrapper script.
	writeFile(filepath.Join(stage, "run-umbravox.sh"), runScript)
	must0(os.Chmod(filepath.Join(stage, "run-umbravox.sh"), 0o755))
	// Symlink umbravox -> run-umbravox.sh.
	must0(os.Symlink("run-umbravox.sh", filepath.Join(stage, "umbravox")))

	// Write LINKAGE.txt via the bundled interpreter.
	linkageOut, linkageErr := exec.Command(filepath.Join(stage, "lib", interpBase), "--list", wrappedBin).Output()
	if linkageErr != nil {
		fmt.Fprintf(os.Stderr, "warning: LINKAGE.txt will be empty: bundled interpreter --list failed: %v\n", linkageErr)
	}
	writeFile(filepath.Join(stage, "LINKAGE.txt"), string(linkageOut))

	// Write FILE.txt.
	fileOut := must(exec.Command("file", wrappedBin).Output())
	writeFile(filepath.Join(stage, "FILE.txt"), string(fileOut))

	// Checksums.
	writeContentsSHA256(stage)
	writeScriptSHA256(stage)
	appendToManifest(stage)

	createTarGz(stage, artifact)
	writeArtifactSHA256(artifact)
	return artifact
}

// buildAppImage creates an experimental AppImage scaffold by wrapping the
// Linux binary bundle in an AppDir layout.
func (c *releaseCtx) buildAppImage() string {
	requireCmd("tar")

	// First, build the linux release.
	linuxArtifact := c.buildLinux()

	pkg := fmt.Sprintf("umbravox-%s-linux-x86_64-appimage-scaffold", c.version)
	stage := filepath.Join(c.outDir, pkg)
	artifact := stage + ".tar.gz"

	os.RemoveAll(stage)
	appDir := filepath.Join(stage, "AppDir")
	must0(os.MkdirAll(appDir, 0o755))

	// Unpack the linux artifact into a temp dir, then copy into AppDir.
	tmpDir := must(os.MkdirTemp("", "umbravox-appimage-src.*"))
	defer os.RemoveAll(tmpDir)
	cmd := exec.Command("tar", "-xzf", linuxArtifact, "-C", tmpDir)
	cmd.Stderr = os.Stderr
	must0(cmd.Run())

	// Find the single extracted directory.
	entries := must(os.ReadDir(tmpDir))
	if len(entries) == 0 {
		fatalf("unable to unpack Linux release artifact for AppImage scaffold")
	}
	linuxDir := filepath.Join(tmpDir, entries[0].Name())

	// Copy contents into AppDir.
	cpCmd := exec.Command("cp", "-a", linuxDir+"/.", appDir+"/")
	cpCmd.Stderr = os.Stderr
	must0(cpCmd.Run())

	// Write AppRun.
	writeFile(filepath.Join(appDir, "AppRun"), appRunScript)
	must0(os.Chmod(filepath.Join(appDir, "AppRun"), 0o755))

	// Write .desktop file.
	writeFile(filepath.Join(appDir, "umbravox.desktop"), fmt.Sprintf(
		"[Desktop Entry]\nType=Application\nName=UmbraVOX\nExec=AppRun\nIcon=umbravox\nTerminal=true\nCategories=Network;Chat;\nComment=Experimental AppImage scaffold for UmbraVOX\n"))

	c.writeManifestLinux(stage, "linux-x86_64", "experimental-appimage-scaffold")
	writeFile(filepath.Join(stage, "APPIMAGE-PLACEHOLDER.txt"), noteAppImage)

	// Checksums scoped to AppDir.
	var paths []string
	filepath.WalkDir(appDir, func(p string, d fs.DirEntry, err error) error {
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
		fmt.Fprintf(&sb, "%s  %s\n", h, rel)
	}
	writeFile(filepath.Join(stage, "CONTENTS.SHA256"), sb.String())
	writeScriptSHA256(stage)

	// Append extra manifest fields.
	f := must(os.OpenFile(filepath.Join(stage, "RELEASE-MANIFEST.txt"), os.O_APPEND|os.O_WRONLY, 0o644))
	fmt.Fprintln(f, "artifact_kind_note=experimental_appimage_scaffold")
	fmt.Fprintln(f, "contents_sha256_file=CONTENTS.SHA256")
	fmt.Fprintln(f, "release_script_sha256_file=RELEASE-SCRIPT.SHA256")
	f.Close()

	createTarGz(stage, artifact)
	writeArtifactSHA256(artifact)
	return artifact
}

// buildPlatform creates a platform source+binary release tarball for freebsd,
// openbsd, netbsd, illumos, or linux-arm64, matching
// scripts/release-package-platform.sh.
func (c *releaseCtx) buildPlatform(platform string) string {
	requireCmd("git")
	requireCmd("tar")

	arch := platformArch(platform)
	pkg := fmt.Sprintf("umbravox-%s-%s-%s", c.version, platform, arch)
	stage := filepath.Join(c.outDir, pkg)
	artifact := stage + ".tar.gz"

	fmt.Fprintf(os.Stderr, "\n  Platform:  %s %s\n  Version:   %s\n  Commit:    %s\n  Artifact:  %s\n\n",
		platformOS(platform), arch, c.version, c.commit, artifact)

	os.RemoveAll(stage)
	stageSourceTree(stage)
	copyCommonDocs(c.root, stage)
	c.writePlatformManifest(stage, platform, arch, "platform-source-release")

	noteFile := fmt.Sprintf("BUILDING-%s.txt", strings.ToUpper(platform))
	writeFile(filepath.Join(stage, noteFile), platformBuildNote(platform))

	// Copy ./uv bootstrap into the release if present.
	uvSrc := filepath.Join(c.root, "uv")
	if info, err := os.Stat(uvSrc); err == nil && !info.IsDir() {
		uvDst := filepath.Join(stage, "uv")
		copyFile(uvSrc, uvDst)
		must0(os.Chmod(uvDst, 0o755))
	}

	tryCopyBinary(stage)

	writeContentsSHA256(stage)
	writePlatformScriptSHA256(stage, c.root)
	appendPlatformManifestFooter(stage)

	createTarGz(stage, artifact)
	writeArtifactSHA256(artifact)
	return artifact
}

// writePlatformManifest writes a manifest with platform/arch fields.
func (c *releaseCtx) writePlatformManifest(stage, platform, arch, kind string) {
	lines := []string{
		"name=UmbraVOX",
		"platform=" + platform,
		"arch=" + arch,
		"artifact_kind=" + kind,
		"version=" + c.version,
		"commit=" + c.commit,
		"timestamp_utc=" + c.stamp,
		fmt.Sprintf("builder=%s/%s", runtime.GOOS, runtime.GOARCH),
	}
	writeFile(filepath.Join(stage, "RELEASE-MANIFEST.txt"), strings.Join(lines, "\n")+"\n")
}

// tryCopyBinary attempts to locate the built umbravox binary via cabal or
// dist-newstyle, copying it into stage/bin/umbravox.  Falls back to a
// source-only note when no binary is found.
func tryCopyBinary(stage string) {
	binDir := filepath.Join(stage, "bin")

	// Try cabal list-bin first.
	if out, err := exec.Command("cabal", "list-bin", "exe:umbravox").Output(); err == nil {
		bin := strings.TrimSpace(string(out))
		if bin != "" {
			if info, err2 := os.Stat(bin); err2 == nil && !info.IsDir() {
				must0(os.MkdirAll(binDir, 0o755))
				dst := filepath.Join(binDir, "umbravox")
				copyFile(bin, dst)
				fileDesc := platformFileDesc(dst)
				fmt.Fprintf(os.Stderr, "  binary: bin/umbravox (%s)\n", fileDesc)
				appendManifestLine(stage, "binary=bin/umbravox")
				return
			}
		}
	}

	// Fallback: search dist-newstyle.
	out, err := exec.Command("find", "dist-newstyle", "-path", "*/build/*/umbravox/umbravox", "-type", "f").Output()
	if err == nil {
		for _, line := range strings.Split(strings.TrimSpace(string(out)), "\n") {
			if line == "" {
				continue
			}
			if info, err2 := os.Stat(line); err2 == nil && !info.IsDir() {
				must0(os.MkdirAll(binDir, 0o755))
				dst := filepath.Join(binDir, "umbravox")
				copyFile(line, dst)
				fileDesc := platformFileDesc(dst)
				fmt.Fprintf(os.Stderr, "  binary: bin/umbravox (%s)\n", fileDesc)
				appendManifestLine(stage, "binary=bin/umbravox")
				return
			}
		}
	}

	fmt.Fprintln(os.Stderr, "  binary: not found (source-only release)")
	appendManifestLine(stage, "binary=none")
}

// appendManifestLine appends a single key=value line to RELEASE-MANIFEST.txt.
func appendManifestLine(stage, line string) {
	f := must(os.OpenFile(filepath.Join(stage, "RELEASE-MANIFEST.txt"), os.O_APPEND|os.O_WRONLY, 0o644))
	defer f.Close()
	fmt.Fprintln(f, line)
}

// appendPlatformManifestFooter appends the checksum file references to the manifest.
func appendPlatformManifestFooter(stage string) {
	f := must(os.OpenFile(filepath.Join(stage, "RELEASE-MANIFEST.txt"), os.O_APPEND|os.O_WRONLY, 0o644))
	defer f.Close()
	fmt.Fprintln(f, "contents_sha256_file=CONTENTS.SHA256")
	fmt.Fprintln(f, "release_script_sha256_file=RELEASE-SCRIPT.SHA256")
}

// writePlatformScriptSHA256 hashes scripts/release-package-platform.sh (the
// shell predecessor) so provenance is recorded, falling back to this binary.
func writePlatformScriptSHA256(stage, root string) {
	scriptPath := filepath.Join(root, "scripts", "release-package-platform.sh")
	if _, err := os.Stat(scriptPath); err != nil {
		// Fall back to the Go executable itself.
		if self, err2 := os.Executable(); err2 == nil {
			scriptPath = self
		} else {
			scriptPath = "tools/cmd/release/main.go"
		}
	}
	h := fileSHA256(scriptPath)
	writeFile(filepath.Join(stage, "RELEASE-SCRIPT.SHA256"), fmt.Sprintf("%s  %s\n", h, scriptPath))
}

// platformFileDesc runs `file` on a binary and returns the description after
// the colon, or an empty string if file is unavailable.
func platformFileDesc(binPath string) string {
	if out, err := exec.Command("file", binPath).Output(); err == nil {
		parts := strings.SplitN(string(out), ":", 2)
		if len(parts) == 2 {
			return strings.TrimSpace(parts[1])
		}
	}
	return ""
}

// ---------- platform metadata ----------

func platformArch(platform string) string {
	switch platform {
	case "linux-arm64":
		return "aarch64"
	default:
		return "amd64"
	}
}

func platformOS(platform string) string {
	switch platform {
	case "freebsd":
		return "FreeBSD"
	case "openbsd":
		return "OpenBSD"
	case "netbsd":
		return "NetBSD"
	case "illumos":
		return "illumos"
	case "linux-arm64":
		return "Linux"
	default:
		return "unknown"
	}
}

func platformBuildNote(platform string) string {
	switch platform {
	case "freebsd":
		return notePlatformFreeBSD
	case "openbsd":
		return notePlatformOpenBSD
	case "netbsd":
		return notePlatformNetBSD
	case "illumos":
		return notePlatformIllumos
	case "linux-arm64":
		return notePlatformLinuxARM64
	default:
		return fmt.Sprintf("UmbraVOX platform release (%s)\n", platform)
	}
}

// writeManifestLinux writes a manifest with ABI fields for linux targets.
func (c *releaseCtx) writeManifestLinux(stage, target, kind string) {
	lines := []string{
		"name=UmbraVOX",
		"target=" + target,
		"artifact_kind=" + kind,
		"version=" + c.version,
		"commit=" + c.commit,
		"timestamp_utc=" + c.stamp,
		fmt.Sprintf("builder=%s/%s", runtime.GOOS, runtime.GOARCH),
		"abi_arch=" + c.abiArch,
		"abi_kernel_min=" + c.abiKernelMin,
		"abi_glibc_min=" + c.abiGlibcMin,
	}
	writeFile(filepath.Join(stage, "RELEASE-MANIFEST.txt"), strings.Join(lines, "\n")+"\n")
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
	out, err := exec.Command("git", "status", "--porcelain").Output()
	if err != nil {
		fatalf("refusing release: git status failed: %v", err)
	}
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

// ---------- linux helpers ----------

// requireCmd checks that a command is available on PATH.
func requireCmd(name string) {
	if _, err := exec.LookPath(name); err != nil {
		fatalf("missing required command: %s", name)
	}
}

// copyCommonDocs copies standard documentation files into the stage directory.
func copyCommonDocs(root, stage string) {
	must0(os.MkdirAll(filepath.Join(stage, "doc"), 0o755))
	for _, name := range []string{"README.md", "LICENSE", "LEGAL-NOTICE.md"} {
		src := filepath.Join(root, name)
		if _, err := os.Stat(src); err == nil {
			copyFile(src, filepath.Join(stage, name))
		}
	}
	if _, err := os.Stat(filepath.Join(root, "PUBLISHING-NOTE.md")); err == nil {
		copyFile(filepath.Join(root, "PUBLISHING-NOTE.md"), filepath.Join(stage, "PUBLISHING-NOTE.md"))
	}
	for _, name := range []string{"QUICKSTART.md", "RELEASES.md"} {
		src := filepath.Join(root, "doc", name)
		if _, err := os.Stat(src); err == nil {
			copyFile(src, filepath.Join(stage, "doc", name))
		}
	}
}

// lddAbsolutePaths runs ldd on the given binary and returns all absolute paths
// found in the output.
func lddAbsolutePaths(binPath string) []string {
	out, err := exec.Command("ldd", binPath).Output()
	if err != nil {
		return nil
	}
	seen := make(map[string]bool)
	var result []string
	for _, field := range strings.Fields(string(out)) {
		if strings.HasPrefix(field, "/") {
			if _, err := os.Stat(field); err == nil && !seen[field] {
				seen[field] = true
				result = append(result, field)
			}
		}
	}
	sort.Strings(result)
	return result
}

// copyDep copies a shared library into stage/lib if not already present.
func copyDep(stage, dep string) {
	base := filepath.Base(dep)
	dst := filepath.Join(stage, "lib", base)
	if _, err := os.Stat(dst); err == nil {
		return
	}
	copyFile(dep, dst)
	// Ensure writable so patchelf etc. can modify if needed.
	os.Chmod(dst, 0o755)
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
  linux           linux native binary bundle
  windows-cli     windows source archive (.zip)
  macos-terminal  macOS source archive (.tar.gz)
  bsd-terminal    BSD source archive (.tar.gz)
  freedos         FreeDOS source archive (.zip)
  appimage        AppImage scaffold
  freebsd         FreeBSD 14.x amd64 platform release
  openbsd         OpenBSD 7.x amd64 platform release
  netbsd          NetBSD 10.x amd64 platform release
  illumos         OmniOS/illumos amd64 platform release
  linux-arm64     Linux aarch64 platform release
  all             build all targets`

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

const noteLinux = `UmbraVOX Linux release

This bundle is built on Linux x86_64 from the current nix-shell and repackages
the built executable with a local ELF interpreter and shared-library set.

It is intended for Linux terminal use on compatible x86_64 systems.
It is not a static binary and it is not a Windows, BSD, macOS, or DOS binary.
`

const noteAppImage = `UmbraVOX experimental AppImage scaffold

This package is a non-authoritative AppImage track scaffold.
It packages the current Linux bundle contents into an AppDir-style layout,
but it does not yet claim the status of a maintained, supported AppImage
release artifact.

Smoke validation against this target is intentionally placeholder-only until
the AppImage support policy and parity evidence are proven.
`

const runScript = `#!/usr/bin/env bash
set -euo pipefail
HERE="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
LOADER="$(find "$HERE/lib" -maxdepth 1 -type f \( -name 'ld-linux*.so*' -o -name 'ld-musl-*.so*' \) | head -n1)"
if [[ -z "${LOADER:-}" ]]; then
  echo "missing bundled dynamic loader under $HERE/lib" >&2
  exit 127
fi
exec "$LOADER" --library-path "$HERE/lib" "$HERE/bin/umbravox.bin" "$@"
`

const appRunScript = `#!/usr/bin/env bash
set -euo pipefail
HERE="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
exec "$HERE/run-umbravox.sh" "$@"
`

const notePlatformFreeBSD = `UmbraVOX FreeBSD release

This tarball contains the UmbraVOX source tree and build notes for FreeBSD.
A native binary is included when this package is built inside a FreeBSD VM
runner; otherwise it is a source release requiring a native build.

Recommended native build on FreeBSD 14.x amd64:
  pkg install ghc cabal-hs gmake git curl ca_root_nss
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

The in-guest build+test script is at scripts/vm-build-test.sh.
`

const notePlatformOpenBSD = `UmbraVOX OpenBSD release

This tarball contains the UmbraVOX source tree and build notes for OpenBSD.
A native binary is included when this package is built inside an OpenBSD VM
runner; otherwise it is a source release requiring a native build.

Recommended native build on OpenBSD 7.x amd64:
  pkg_add ghc cabal-install
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

The in-guest build+test script is at scripts/vm-build-test.sh.
`

const notePlatformNetBSD = `UmbraVOX NetBSD release

This tarball contains the UmbraVOX source tree and build notes for NetBSD.
A native binary is included when this package is built inside a NetBSD VM
runner; otherwise it is a source release requiring a native build.

Recommended native build on NetBSD 10.x amd64:
  pkgin install ghc cabal-install
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

The in-guest build+test script is at scripts/vm-build-test.sh.
`

const notePlatformIllumos = `UmbraVOX illumos/OmniOS release

This tarball contains the UmbraVOX source tree and build notes for
OmniOS CE (illumos). A native binary is included when this package is
built inside an OmniOS VM runner; otherwise it is a source release.

Recommended native build on OmniOS CE r151052 amd64:
  pkg set-publisher -g https://pkg.ooce.omnios.org/omnios/r151052 ooce
  pkg install ooce/lang/ghc ooce/developer/cabal developer/gcc14 \
      developer/gnu-binutils system/header developer/build/gnu-make \
      scm/git web/curl
  export PATH="/opt/ooce/bin:/opt/ooce/sbin:$PATH"
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

The in-guest build+test script is at scripts/vm-build-test.sh.
`

const notePlatformLinuxARM64 = `UmbraVOX Linux arm64 release

This tarball contains the UmbraVOX source tree and build notes for
Linux aarch64. A native binary is included when this package is built
on an arm64 runner (native or QEMU emulation); otherwise it is a source release.

Recommended native build on Linux aarch64 (Debian/Ubuntu example):
  apt-get install -y ghc cabal-install make git curl
  cabal update
  cabal build all
  cabal test umbravox-test --test-options=required

Alternatively, use ghcup for the latest GHC release (download, verify,
then execute — never pipe curl directly to sh):
  curl --proto '=https' --tlsv1.2 -sSf -o /tmp/ghcup-install.sh https://get-ghcup.haskell.org
  # TODO: Replace with current ghcup installer SHA-256 from https://www.haskell.org/ghcup/
  echo "TODO_INSERT_ACTUAL_SHA256  /tmp/ghcup-install.sh" | sha256sum -c -
  sh /tmp/ghcup-install.sh
  rm /tmp/ghcup-install.sh

The in-guest build+test script is at scripts/vm-build-test.sh.
`
