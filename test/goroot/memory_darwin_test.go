//go:build darwin

package goroot

import (
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"testing"
)

func systemMemoryMonitoringSupported() bool { return true }

func readSystemMemoryState() (systemMemoryState, error) {
	pressureOutput, err := exec.Command("memory_pressure", "-Q").Output()
	if err != nil {
		return systemMemoryState{}, err
	}
	const freePrefix = "System-wide memory free percentage: "
	freePercent := -1
	for _, line := range strings.Split(string(pressureOutput), "\n") {
		if !strings.HasPrefix(line, freePrefix) {
			continue
		}
		value := strings.TrimSuffix(strings.TrimSpace(strings.TrimPrefix(line, freePrefix)), "%")
		freePercent, err = strconv.Atoi(value)
		if err != nil {
			return systemMemoryState{}, fmt.Errorf("parse memory_pressure line %q: %w", line, err)
		}
		break
	}
	if freePercent < 0 {
		return systemMemoryState{}, fmt.Errorf("memory_pressure did not report a free percentage")
	}

	swapOutput, err := exec.Command("sysctl", "-n", "vm.swapusage").Output()
	if err != nil {
		return systemMemoryState{}, err
	}
	swapFree, swapPresent, err := parseDarwinSwapUsage(swapOutput)
	if err != nil {
		return systemMemoryState{}, err
	}
	return systemMemoryState{freePercent: freePercent, swapFree: swapFree, swapPresent: swapPresent}, nil
}

func parseDarwinSwapUsage(output []byte) (swapFree uint64, swapPresent bool, err error) {
	fields := strings.Fields(string(output))
	values := make(map[string]uint64, 2)
	for i := 0; i+2 < len(fields); i++ {
		name := fields[i]
		if (name != "total" && name != "free") || fields[i+1] != "=" {
			continue
		}
		value, parseErr := parseDarwinMemorySize(fields[i+2])
		if parseErr != nil {
			return 0, false, parseErr
		}
		values[name] = value
	}
	total, hasTotal := values["total"]
	free, hasFree := values["free"]
	if !hasTotal || !hasFree {
		return 0, false, fmt.Errorf("sysctl vm.swapusage did not report total and free swap: %q", output)
	}
	return free, total > 0, nil
}

func parseDarwinMemorySize(value string) (uint64, error) {
	if len(value) < 2 {
		return 0, fmt.Errorf("invalid Darwin memory size %q", value)
	}
	multiplier := float64(1)
	switch value[len(value)-1] {
	case 'K':
		multiplier = 1 << 10
	case 'M':
		multiplier = 1 << 20
	case 'G':
		multiplier = 1 << 30
	case 'T':
		multiplier = 1 << 40
	default:
		return 0, fmt.Errorf("invalid Darwin memory size %q", value)
	}
	number, err := strconv.ParseFloat(value[:len(value)-1], 64)
	if err != nil {
		return 0, fmt.Errorf("parse Darwin memory size %q: %w", value, err)
	}
	return uint64(number * multiplier), nil
}

func TestParseDarwinSwapUsage(t *testing.T) {
	tests := []struct {
		name        string
		output      string
		wantFree    uint64
		wantPresent bool
		wantErr     bool
	}{
		{
			name:        "active swap",
			output:      "total = 2048.00M  used = 512.00M  free = 1.50G  (encrypted)",
			wantFree:    1536 << 20,
			wantPresent: true,
		},
		{
			name:   "runner without swap",
			output: "total = 0.00M  used = 0.00M  free = 0.00M",
		},
		{
			name:    "missing total",
			output:  "used = 0.00M free = 0.00M",
			wantErr: true,
		},
		{
			name:    "invalid free",
			output:  "total = 1.00G free = unknown",
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotFree, gotPresent, err := parseDarwinSwapUsage([]byte(tt.output))
			if (err != nil) != tt.wantErr {
				t.Fatalf("parseDarwinSwapUsage() error = %v, wantErr %v", err, tt.wantErr)
			}
			if gotFree != tt.wantFree || gotPresent != tt.wantPresent {
				t.Fatalf("parseDarwinSwapUsage() = (%d, %v), want (%d, %v)", gotFree, gotPresent, tt.wantFree, tt.wantPresent)
			}
		})
	}
}
