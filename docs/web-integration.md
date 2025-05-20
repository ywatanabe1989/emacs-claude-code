# Claude VTerm Web Integration

This document describes how to use the web browsing capabilities integrated with Claude VTerm mode in Emacs.

## Overview

The web integration allows you to:

1. Browse the web directly within Emacs using text-based browsers
2. Search the web and view results without leaving Emacs
3. Send web content directly to Claude for analysis or discussion
4. Integrate web research with AI assistance seamlessly

## Available Browsers

The Apptainer environment provides multiple text-based web browsers:

1. **EWW (Emacs Web Wowser)** - Built into Emacs
2. **W3M** - A text-based web browser with advanced features
3. **Elpher** - A Gemini and Gopher protocol client
4. **Lynx/Links/ELinks** - Classic text-based browsers (available in terminal)

## Key Bindings

| Key Binding | Function | Description |
|-------------|----------|-------------|
| `C-c C-w` | `eww` | Open EWW browser |
| `C-c C-s` | `eww-search-words` | Search the web using region or prompt |
| `C-c C-b` | `ecc-browse-and-send-to-claude` | Browse a URL and send contents to Claude |
| `C-c o` or `C-c C-o` | `eww-browse-with-external-browser` | Open current page in external browser |

## Using the Web Integration

### Basic Web Browsing

1. Press `C-c C-w` to open the EWW browser
2. Enter a URL when prompted, e.g., `https://example.com`
3. Navigate using standard Emacs commands:
   - `TAB` to move between links
   - `RET` to follow a link
   - `l` to go back
   - `r` to reload the page
   - `q` to quit

### Web Searching

1. Press `C-c C-s` to search the web
2. Type your search query or use the selected region
3. Results will appear in the EWW buffer

### Sending Web Content to Claude

There are two ways to send web content to Claude:

#### Method 1: Integrated Browsing + Sending

1. Press `C-c C-b`
2. Enter the URL you want to browse and send to Claude
3. The content will automatically be sent to Claude after loading

#### Method 2: Manual Copy/Paste

1. Browse to a page using `C-c C-w`
2. Select the content you want to send (using standard Emacs region commands)
3. Switch to the Claude VTerm buffer
4. Paste the content (in vterm: `C-c C-y`)

### Switching Between Claude and Web Browsing

You can easily switch between Claude and web browsing:

1. Use `C-x b` to switch between buffers
2. Use `C-c C-v` to return to Claude if needed

## Advanced Features

### Customizing Web Search Engine

You can change the default search engine by customizing `eww-search-prefix`:

```elisp
;; For DuckDuckGo
(setq eww-search-prefix "https://duckduckgo.com/html?q=")

;; For Google
(setq eww-search-prefix "https://www.google.com/search?q=")

;; For Brave Search (default)
(setq eww-search-prefix "https://search.brave.com/search?q=")
```

### W3M Advanced Features

If you prefer W3M over EWW:

1. Open a terminal in the Apptainer environment (`./launch-claude-emacs.sh --shell`)
2. Run `w3m` for a more feature-rich browsing experience

### Research Workflow Integration

For comprehensive research:

1. Open multiple EWW buffers to compare sources
2. Use `ecc-browse-and-send-to-claude` to gather information
3. Ask Claude to analyze, compare, or summarize the information
4. Save the results to a file for later reference

## Troubleshooting

Common issues and solutions:

### Pages Not Rendering Correctly

Text-based browsers have limitations with complex layouts and JavaScript:

- Try a different URL
- Use `C-c o` to open in an external browser if needed
- Focus on text-heavy sites for best results

### Connection Issues

If you encounter connection problems:

1. Check your internet connection
2. Verify you're not behind a proxy requiring authentication
3. Try using the `--net` flag when running the Apptainer container

### Large Pages

For very large pages that slow down EWW:

1. Use a more lightweight browser like Lynx
2. Use the external command-line tools to filter content first