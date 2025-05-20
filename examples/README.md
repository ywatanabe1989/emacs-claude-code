# Emacs Claude Code Examples

This directory contains examples showing how to use the various features of the emacs-claude-code package.

## Basic Usage

- [basic-usage.el](./basic-usage.el) - Getting started with sending prompts to Claude and handling responses

## Buffer Management

- [buffer/buffer-management.el](./buffer/buffer-management.el) - Working with multiple Claude buffers, navigation, and cleanup
- [buffer/buffer-customization.el](./buffer/buffer-customization.el) - Customizing buffer creation, naming, and behavior

## State Management

- [state/state-management.el](./state/state-management.el) - Detecting and handling different Claude states and automating interactions

## Template Usage

- [template/template-usage.el](./template/template-usage.el) - Using built-in templates, creating custom templates, and managing the template cache

## VTerm Integration

- [term/vterm-integration.el](./term/vterm-integration.el) - Using Claude with vterm for optimized terminal interaction

## Command and Integration

- [command-integration.el](./command-integration.el) - Creating custom commands and integrating Claude into your workflow

## Using the Examples

To use any of these examples, you can:

1. Open the desired example file in Emacs
2. Evaluate the file with `M-x eval-buffer`
3. Call any of the example functions interactively with `M-x`

For instance, after loading `basic-usage.el`, you can run:

```
M-x example-run-claude-demo
```

## Example Function Naming Convention

All example functions follow the naming convention:

```
example-<module>-<feature>
```

For example:
- `example-claude-simple-prompt`
- `example-create-buffer-dashboard`
- `example-template-selector`

## Creating Your Own Extensions

These examples serve as starting points for your own customizations. You can:

1. Copy and modify the examples to fit your needs
2. Use the provided functions as templates for your own implementations
3. Combine multiple examples to create more complex workflows

Feel free to explore the examples and adapt them to your own use cases!