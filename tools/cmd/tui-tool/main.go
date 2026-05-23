// Command tui-tool provides TUI testing and capture utilities for UmbraVOX.
//
// This replaces:
//   - scripts/tui-screenshot-local.sh
//   - scripts/vm-screenshot-capture.sh
//   - scripts/vm-record-session.sh
//   - scripts/vm-visual-regression.sh
//   - scripts/vm-tui-scenario.sh
//
// Usage:
//
//	tui-tool screenshot [--output file.png]    Capture a TUI screenshot
//	tui-tool capture [--format png|sixel]      Capture TUI output
//	tui-tool record [--output file.cast]       Record a TUI session (asciinema)
//	tui-tool regression [--baseline dir]       Run visual regression tests
package main

import (
	"flag"
	"fmt"
	"os"
)

const usage = `tui-tool — UmbraVOX TUI testing and capture utilities

Usage:
  tui-tool <subcommand> [flags]

Subcommands:
  screenshot    Capture a TUI screenshot
  capture       Capture TUI output in various formats
  record        Record a TUI session (asciinema format)
  regression    Run visual regression tests against baselines

Flags for 'screenshot':
  --output <file>    Output file path (default: screenshot.png)

Flags for 'capture':
  --format <fmt>     Output format: png, sixel, text (default: png)
  --output <file>    Output file path

Flags for 'record':
  --output <file>    Output file path (default: session.cast)
  --scenario <name>  TUI scenario to run

Flags for 'regression':
  --baseline <dir>   Baseline screenshots directory
  --threshold <n>    Pixel difference threshold (default: 0.01)
`

func main() {
	flag.Usage = func() { fmt.Fprint(os.Stderr, usage) }
	flag.Parse()

	args := flag.Args()
	if len(args) == 0 {
		flag.Usage()
		os.Exit(2)
	}

	subcmd := args[0]
	switch subcmd {
	case "screenshot":
		ssFS := flag.NewFlagSet("screenshot", flag.ExitOnError)
		output := ssFS.String("output", "screenshot.png", "output file path")
		ssFS.Parse(args[1:])

		fmt.Println("[tui-tool] screenshot: not yet implemented")
		fmt.Printf("[tui-tool] output: %s\n", *output)
		fmt.Println("[tui-tool] This will capture a TUI screenshot.")
	case "capture":
		capFS := flag.NewFlagSet("capture", flag.ExitOnError)
		format := capFS.String("format", "png", "output format: png, sixel, text")
		output := capFS.String("output", "", "output file path")
		capFS.Parse(args[1:])

		fmt.Println("[tui-tool] capture: not yet implemented")
		fmt.Printf("[tui-tool] format: %s\n", *format)
		if *output != "" {
			fmt.Printf("[tui-tool] output: %s\n", *output)
		}
		fmt.Println("[tui-tool] This will capture TUI output.")
	case "record":
		recFS := flag.NewFlagSet("record", flag.ExitOnError)
		output := recFS.String("output", "session.cast", "output file path")
		scenario := recFS.String("scenario", "", "TUI scenario to run")
		recFS.Parse(args[1:])

		fmt.Println("[tui-tool] record: not yet implemented")
		fmt.Printf("[tui-tool] output: %s\n", *output)
		if *scenario != "" {
			fmt.Printf("[tui-tool] scenario: %s\n", *scenario)
		}
		fmt.Println("[tui-tool] This will record a TUI session in asciinema format.")
	case "regression":
		regFS := flag.NewFlagSet("regression", flag.ExitOnError)
		baseline := regFS.String("baseline", "", "baseline screenshots directory")
		threshold := regFS.Float64("threshold", 0.01, "pixel difference threshold")
		regFS.Parse(args[1:])

		fmt.Println("[tui-tool] regression: not yet implemented")
		if *baseline != "" {
			fmt.Printf("[tui-tool] baseline: %s\n", *baseline)
		}
		fmt.Printf("[tui-tool] threshold: %.4f\n", *threshold)
		fmt.Println("[tui-tool] This will run visual regression tests against baselines.")
	case "help", "-h", "--help":
		flag.Usage()
	default:
		fmt.Fprintf(os.Stderr, "tui-tool: unknown subcommand %q\n\n", subcmd)
		flag.Usage()
		os.Exit(2)
	}
}
