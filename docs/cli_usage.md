## CLI Usage

The Steel CLI supports several modes and flags:

```bash
steel_lang <source_file> [mode] [flags]

```

| Argument      | Description                                                  |
|---------------|--------------------------------------------------------------|
| `source_file` | Path to the `.steel` source file.                            |
| `run`         | (Default) Compiles and executes the script.                  |
| `check`       | Runs type-checking only (no execution).                      |
| `parse`       | Debugs the parser and outputs the AST.                       |
| `-d`          | **Debug Mode:** Prints disassembly and AST info.             |
| `-f`          | **Force Mode:** Attempts to run even if errors are detected. |
