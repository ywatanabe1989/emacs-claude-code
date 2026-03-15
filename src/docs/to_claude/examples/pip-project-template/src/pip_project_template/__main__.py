#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2025-08-27 01:08:52 (ywatanabe)"
# File: /home/ywatanabe/proj/pip-project-template/src/__main__.py
# ----------------------------------------
from __future__ import annotations
import os
__FILE__ = (
    "./pip-project-template/src/__main__.py"
)
__DIR__ = os.path.dirname(__FILE__)
# ----------------------------------------
"""Main entry point for CLI."""

import sys

from .cli import main

if __name__ == "__main__":
    sys.exit(main())

# EOF
