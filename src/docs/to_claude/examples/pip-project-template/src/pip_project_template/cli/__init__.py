"""CLI module."""

from ._GlobalArgumentParser import GlobalArgumentParser


def main():
    """Main CLI entry point."""
    import sys
    import importlib

    parser, subparsers_dict = GlobalArgumentParser.get_main_parser()

    if len(sys.argv) == 1:
        parser.print_help()
        return 0

    # Parse known command
    args, remaining = parser.parse_known_args()

    if args.command:
        # Convert command name back to module name (hyphens to underscores)
        module_name = args.command.replace('-', '_')
        
        try:
            # Dynamic import and execution
            module = importlib.import_module(f'.{module_name}', package='pip_project_template.cli')
            if hasattr(module, 'main'):
                return module.main(remaining)
            else:
                print(f"Error: Command '{args.command}' does not have a main function")
                return 1
        except ImportError:
            print(f"Error: Command '{args.command}' not found")
            return 1
    else:
        parser.print_help()
        return 1
