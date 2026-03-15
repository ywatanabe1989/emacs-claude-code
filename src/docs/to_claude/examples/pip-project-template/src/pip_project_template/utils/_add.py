#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2025-08-27 00:57:54 (ywatanabe)"
# File: /home/ywatanabe/proj/pip-project-template/src/utils/_add.py
# ----------------------------------------
from __future__ import annotations
import os
__FILE__ = (
    "./pip-project-template/src/utils/_add.py"
)
__DIR__ = os.path.dirname(__FILE__)
# ----------------------------------------

"""Add function."""


def add(a: float, b: float) -> float:
    return a + b


def main():
    print(f"2 + 3 = {add(2, 3)}")


if __name__ == "__main__":
    main()

# EOF
