#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2025-08-27 00:58:20 (ywatanabe)"
# File: /home/ywatanabe/proj/pip-project-template/src/utils/_multiply.py
# ----------------------------------------
from __future__ import annotations
import os
__FILE__ = (
    "./pip-project-template/src/utils/_multiply.py"
)
__DIR__ = os.path.dirname(__FILE__)
# ----------------------------------------
"""Multiply function."""


def multiply(a: float, b: float) -> float:
    return a * b


def main():
    print(f"4 * 5 = {multiply(4, 5)}")


if __name__ == "__main__":
    main()

# EOF
