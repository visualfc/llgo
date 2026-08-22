package compile

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"

	"github.com/xgo-dev/llgo/internal/clang"
)

type CompileOptions struct {
	CC      string // Compiler to use
	Linker  string
	CCFLAGS []string
	CFLAGS  []string
	LDFLAGS []string
}

type CompileGroup struct {
	OutputFileName string
	Files          []string // List of source files to compile
	CFlags         []string // C compiler flags
	CCFlags        []string
	LDFlags        []string // Linker flags
}

// IsCompiled checks if the compile group has already been compiled by verifying
// if the output archive file exists in the specified directory
func (g CompileGroup) IsCompiled(outputDir string) bool {
	archive := filepath.Join(outputDir, filepath.Base(g.OutputFileName))
	_, err := os.Stat(archive)
	return err == nil
}

// Compile compiles all source files in the group into a static library archive
// If the archive already exists, compilation is skipped
func (g CompileGroup) Compile(
	outputDir string, options CompileOptions,
) (err error) {
	if g.IsCompiled(outputDir) {
		return
	}
	tmpCompileDir, err := os.MkdirTemp("", "compile-group*")
	if err != nil {
		return
	}
	defer os.RemoveAll(tmpCompileDir)

	compileLDFlags := append(slices.Clone(options.LDFLAGS), g.LDFlags...)
	compileCCFlags := append(slices.Clone(options.CCFLAGS), g.CCFlags...)
	compileCFFlags := append(slices.Clone(options.CFLAGS), g.CFlags...)

	cfg := clang.NewConfig(options.CC, compileCCFlags, compileCFFlags, compileLDFlags, options.Linker)

	var objFiles []string

	compiler := clang.NewCompiler(cfg)

	compiler.Verbose = true

	archive := filepath.Join(outputDir, filepath.Base(g.OutputFileName))
	fmt.Fprintf(os.Stderr, "Start to compile group %s to %s...\n", g.OutputFileName, archive)

	for _, file := range g.Files {
		var tempObjFile *os.File
		tempObjFile, err = os.CreateTemp(tmpCompileDir, objectFilePattern(file))
		if err != nil {
			return
		}
		tempObjName := tempObjFile.Name()
		if err = tempObjFile.Close(); err != nil {
			return
		}

		lang := "c"
		if filepath.Ext(file) == ".S" {
			lang = "assembler-with-cpp"
		}
		err = compiler.Compile("-o", tempObjName, "-x", lang, "-c", file)
		if err != nil {
			return
		}

		objFiles = append(objFiles, tempObjName)
	}

	ccDir := filepath.Dir(options.CC)
	llvmAr := filepath.Join(ccDir, "llvm-ar")

	responseFile, err := writeArchiveResponseFile(tmpCompileDir, objFiles)
	if err != nil {
		return err
	}
	// newlib contains hundreds of object files, whose expanded paths exceed
	// Windows' CreateProcess command-line limit. LLVM tools support response
	// files on every host, so keep the object list out of the process command
	// line rather than splitting one archive update into platform-only batches.
	cmd := exec.Command(llvmAr, "rcs", archive, "@"+responseFile)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err = cmd.Run()
	return
}

func writeArchiveResponseFile(dir string, objFiles []string) (string, error) {
	var contents strings.Builder
	for _, objFile := range objFiles {
		// LLVM's response-file parser accepts forward slashes on Windows. Quote
		// each argument so temporary roots containing spaces remain one path.
		contents.WriteByte('"')
		contents.WriteString(filepath.ToSlash(objFile))
		contents.WriteString("\"\n")
	}
	responseFile := filepath.Join(dir, "objects.rsp")
	if err := os.WriteFile(responseFile, []byte(contents.String()), 0o600); err != nil {
		return "", err
	}
	return responseFile, nil
}

func objectFilePattern(source string) string {
	name := filepath.Base(source)
	name = strings.Map(func(r rune) rune {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9', r == '.', r == '-', r == '_':
			return r
		default:
			return '-'
		}
	}, name)
	return fmt.Sprintf("%s-*.o", name)
}

// CompileConfig represents compilation configuration
type CompileConfig struct {
	Groups       []CompileGroup
	ExportCFlags []string
}

type LibConfig struct {
	Url            string
	Name           string // Library name (e.g., "picolibc", "musl", "glibc")
	Version        string
	ResourceSubDir string
}

// String returns a string representation of the library configuration
// in the format "name-version"
func (cfg LibConfig) String() string {
	return fmt.Sprintf("%s-%s", cfg.Name, cfg.Version)
}
