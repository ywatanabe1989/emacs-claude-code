#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2025-08-27 09:18:59 (ywatanabe)"
# File: /home/ywatanabe/proj/pip-project-template/src/minimal_pip_project/core/_Calculator.py
# ----------------------------------------
from __future__ import annotations
import os
__FILE__ = (
    "./src/minimal_pip_project/core/_Calculator.py"
)
__DIR__ = os.path.dirname(__FILE__)
# ----------------------------------------

"""Simple calculator."""

from ..utils import add, multiply


class Calculator:
    """Basic calculator."""

    def calculate(self, a: float, b: float, operation: str = "add") -> float:
        """Perform calculation."""
        if operation == "add":
            return add(a, b)
        elif operation == "multiply":
            return multiply(a, b)
        else:
            raise ValueError(f"Unknown operation: {operation}")


def main():
    """Demo calculator."""
    calculator = Calculator()
    print(calculator.calculate(10, 5, "add"))
    print(calculator.calculate(3, 4, "multiply"))


if __name__ == "__main__":
    main()

# EOF
