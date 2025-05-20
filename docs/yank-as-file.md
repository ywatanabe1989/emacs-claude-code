# Yank-as-File Feature in Claude Code

## Overview

The Yank-as-File feature allows you to easily extract Claude's output into files, automatically detecting the appropriate file format based on the content. This feature is particularly useful when Claude generates code, documentation, or other structured content that you want to save as a separate file for further editing or use.

## Key Features

- **Content-based File Type Detection**: Automatically identifies the appropriate file extension based on content patterns
- **Multiple Save Options**: Save entire buffer or just selected regions
- **Quick Save**: Single-command option for rapid saving with auto-generated filenames
- **Custom Output Directory**: Configure where files are saved by default

## Commands

### Basic Commands

| Command | Keybinding | Description |
|---------|------------|-------------|
| `ecc-vterm-yank-as-file` | `C-c C-f` | Save selected region to a file with prompted filename |
| `ecc-vterm-yank-buffer-as-file` | `C-c C-b` | Save entire buffer content to a file with prompted filename |
| `ecc-vterm-quick-yank-region` | `C-c C-q` | Quickly save selected region with auto-generated filename |

These commands are also available in the Claude-VTerm menu.

## Usage Examples

### Saving a Code Snippet

1. Select the code region in your Claude buffer
2. Press `C-c C-f`
3. Enter a filename or accept the suggested one (which includes detected file extension)
4. The content will be saved to the file, and you'll be asked if you want to open it

### Saving Entire Conversation

1. Press `C-c C-b` to save the entire buffer
2. Choose a filename
3. The content will be saved with the appropriate extension

### Quick Save for Multiple Outputs

When Claude generates multiple code snippets and you want to save them quickly:

1. Select a region
2. Press `C-c C-q`
3. The content is saved with an auto-generated name (timestamp-based)
4. Repeat for other regions

## File Type Detection

The system detects file types based on content patterns. Currently supported types include:

- Python (`.py`)
- JavaScript (`.js`)
- HTML (`.html`)
- CSS (`.css`)
- Emacs Lisp (`.el`)
- Shell scripts (`.sh`)
- Java (`.java`)
- C (`.c`)
- C++ (`.cpp`)
- Ruby (`.rb`)
- PHP (`.php`)
- TypeScript (`.ts`)
- Go (`.go`)
- Rust (`.rs`)
- JSON (`.json`)
- Markdown (`.md`)
- YAML (`.yaml`)
- Plain text (`.txt`)

## Customization

You can customize the yank-as-file behavior through these variables:

- `ecc-vterm-yank-default-dir`: Set a default directory for saved files
- `ecc-vterm-yank-extension-alist`: Customize the patterns used for file type detection

Example:

```elisp
;; Set default save directory
(setq ecc-vterm-yank-default-dir "~/claude-outputs/")

;; Add a custom file type detection pattern
(add-to-list 'ecc-vterm-yank-extension-alist '("sql" . "\\(SELECT\\|INSERT\\|UPDATE\\|DELETE\\|CREATE\\|ALTER\\)"))
```

## Best Practices

- Use `ecc-vterm-yank-as-file` for important code that you want to name specifically
- Use `ecc-vterm-quick-yank-region` during rapid prototyping sessions
- The system will detect file types automatically, but you can always change the extension manually when prompted for a filename