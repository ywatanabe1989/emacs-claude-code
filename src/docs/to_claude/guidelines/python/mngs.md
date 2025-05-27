<!-- ---
!-- Timestamp: 2025-05-20 06:10:37
!-- Author: ywatanabe
!-- File: /ssh:ywatanabe@sp:/home/ywatanabe/.claude/to_claude/guidelines/IMPORTANT-guidelines-programming-Python-MNGS-Rules.md
!-- --- -->

# MNGS Guidelines

## What is MNGS?
- `mngs` means "monogusa" (meaning "lazy" in Japanese)
- A Python utility package designed to standardize scientific analyses and applications
- Located in `~/proj/mngs_repo/src/mngs`
- Remote repository: `git@github.com:ywatanabe1989:mngs`
- Installed via pip in development mode: `pip install -e ~/proj/mngs_repo`

## Bug Report
Since `mngs` is maintained by the user himself and installed via editable mode, `mngs` itself can be, or should be, fixed. When you encountered problems which stems from `mngs` package itself, create bug report to `$HOME/proj/mngs_repo/project_management/bug-report-<title>-<timestamp>.md`(not in the current repository). In this case, follow the guidelines `./docs/to_claude/guidelines/guidelines-programming-Bug-Report-Rules.md`. 

---

## MNGS-based Project Examples
 - For Scientific Project
   - Use `./scripts`
   - See `./docs/to_claude/examples/example-mngs-project`
 - For Python Pip Package Project
   - Use `./src`
   - See `./docs/to_claude/examples/`

## Core Functions

- Use mngs function as much as possible to ensure reproducibility and readability as a whole repository

### Environment Management
```python
mngs.gen.start(...)     # Initialize environment (logging, paths, matplotlib, random seed)
mngs.gen.close(...)     # Clean up environment
```

### Configuration Management
```python
CONFIG = mngs.io.load_configs()   # Load YAML files from ./config as a dot-accessible dictionary
```

### Input/Output Operations
```python
# Load data
data = mngs.io.load('./relative/path.ext')   # Load from project root, auto-detects format

# Save data with automatic format handling and symlink creation
# ALWAYS specify symlink_from_cwd, Always start relative path from `./` 
# DO NOT NEED TO MKDIR as target directories are internally handled
mngs.io.save(obj, './relative/path.ext', symlink_from_cwd=True)
```

### Plotting
IMPORTANT: `mngs.plt.subplots` is a wrapper for `matplotlib.pyplot.plt.subplots`. However, it is not perfect so that when problem occurs, please create a bug-report to `$HOME/proj/mngs_repo/project_management/bug-report-<title>-<timestamp>.md`

```python
# Create trackable plots
fig, axes = mngs.plt.subplots(ncols=2)   # Returns wrapper objects that track plotting data

# Set axis properties with combined method
ax.set_xyt('X-axis', 'Y-axis', 'Title')  # ALWAYS use mngs wrappers instead of matplotlib methods
```

### Utility Functions
```python
mngs.str.printc("message here", c='blue', char="-", n=40)      # Colored console output with specified border

``` plaintext
----------------------------------------
message here
----------------------------------------
```
mngs.stats.p2stars(p_value)               # Convert p-values to significance stars (e.g., *)
mngs.stats.fdr_correction(results_df)     # Apply FDR correction for multiple comparisons
mngs.pd.round(df, factor=3)               # Round numeric values in dataframe
```

---

## Directory Structure

```
<project root>
│
├── config/                 # Configuration files
│   └── *.yaml              # YAML config files (PATH.yaml, etc.)
│
├── data/                   # Centralized data storage
│   └── <dir_name>/         # Organized by category
│        └── file.ext → ../../scripts/<script>_out/file.ext  # Symlinks to script outputs
│
└── scripts/                # Script files and outputs
    └── <category>/
        ├── script.py       # Python script
        └── script_out/     # Output directory for this script
            ├── file.ext    # Output files
            └── logs/       # Logging directory for each run (managed by `mngs.gen.start` and `mngs.gen.close`)
                ├── RUNNING
                ├── FINISHED_SUCCESS
                └── FINISHED_FAILURE
```

> **IMPORTANT**: DO NOT CREATE DIRECTORIES IN PROJECT ROOT  
> Create child directories under predefined directories instead

---

## Detailed Module Reference

### `mngs.io` Module

#### Loading Data

```python
# Load data with automatic format detection
data = mngs.io.load('./data/results.csv')  # CSV file, using pandas
config = mngs.io.load('./config/params.yaml')  # YAML file
array = mngs.io.load('./data/features.npy')  # NumPy array
```

#### Supported File Extensions

**For `mngs.io.load()`:**

| Category | Extensions |
|----------|------------|
| **Numeric Data** | `.npy`, `.npz`, `.mat`, `.h5`, `.hdf5` |
| **Tabular Data** | `.csv`, `.xlsx`, `.xls`, `.tsv` |
| **Text & Config** | `.json`, `.yaml`, `.yml`, `.xml`, `.txt` |
| **Python Objects** | `.pkl`, `.pickle`, `.joblib` |
| **Media** | `.jpg`, `.png`, `.gif`, `.tiff`, `.pdf`, `.mp3`, `.wav` |
| **Documents** | `.docx`, `.pdf` |
| **Special** | `.db`, `.sqlite3`, `.edf` (EEG data) |

**For `mngs.io.save()`:**

| Category | Extensions |
|----------|------------|
| **Numeric Data** | `.npy`, `.npz`, `.mat`, `.h5`, `.hdf5` |
| **Tabular Data** | `.csv`, `.xlsx`, `.tsv` |
| **Text & Config** | `.json`, `.yaml`, `.yml`, `.txt` |
| **Python Objects** | `.pkl`, `.pickle`, `.joblib` |
| **Media** | `.jpg`, `.png`, `.gif`, `.tiff`, `.mp4`, `.html` |
| **Visualizations** | `.png`, `.jpg`, `.svg`, `.pdf`, `.html` |

> **⚠️ ALWAYS explicitly specify `symlink_from_cwd=True` or `symlink_from_cwd=False`**

`.jpg` is the first option for images.

#### I/O Reversibility

Objects saved with `mngs.io.save()` can be loaded with `mngs.io.load()` while maintaining their original structure:

```python
# Save any Python object
data = {"name": "example", "values": np.array([1, 2, 3])}
mngs.io.save(data, './data/example.pkl', symlink_from_cwd=True)

# Load it back with identical structure
loaded_data = mngs.io.load('./data/example.pkl')
```

This reversibility ensures data integrity throughout your workflow.

#### Special Cases Handling

`mngs.io` automatically handles special object types:

1. **Figure Objects**: When saving matplotlib figures, both the figure image and data are saved:
   ```python
   mngs.io.save(fig, './data/plot.png', symlink_from_cwd=True)
   # Creates both plot.png and plot.csv
   ```

2. **Optuna Studies**: When saving Optuna studies, generates data and visualization files:
   ```python
   mngs.io.save(study, './data/optuna_study', symlink_from_cwd=True)
   # Creates CSV and PNG files
   ```

3. **DataFrames List**: When saving a list of DataFrames, saves each separately:
   ```python
   mngs.io.save([df1, df2, df3], './data/dataframes', symlink_from_cwd=True)
   ```

#### Loading Configurations

```python
# Load all YAML files from ./config
CONFIG = mngs.io.load_configs()

# Access configuration values
print(CONFIG.PATH.DATA)  # Access path defined in PATH.yaml

# Resolve f-strings in config
patient_id = "001"
data_path = eval(CONFIG.PATH.PATIENT_DATA)  # f"./data/patient_{patient_id}/data.csv"
```

### `mngs.plt` Module

#### Creating Plots

```python
# Create a figure with tracked axes
fig, axes = mngs.plt.subplots(ncols=2, figsize=(10, 5))

# Plot data
axes[0].plot(x, y, label='Data')
axes[1].scatter(x, z, label='Scatter')

# Set labels and title using mngs wrapper method (PREFERRED WAY)
axes[0].set_xyt('X-axis', 'Y-axis', 'Data Plot')
axes[1].set_xyt('X-axis', 'Y-axis', 'Scatter Plot')

# Add legend
for ax in axes:
    ax.legend()
```

#### Exporting Plot Data

```python
# Automatically export to CSV when saving figure
mngs.io.save(fig, './data/figures/plot.png', symlink_from_cwd=True)
# Creates:
# - /path/to/script_out/data/figures/plot.png
# - /path/to/script_out/data/figures/plot.csv
# - ./data/figures/plot.png -> /path/to/script_out/data/figures/plot.png
# - ./data/figures/plot.csv -> /path/to/script_out/data/figures/plot.csv

# Or manually export data
fig.export_as_csv('./data/csv/plot_data.csv')
```

#### Supported Plot Types for CSV Export

- Line plots (`ax.plot()`)
- Scatter plots (`ax.scatter()`)
- Bar plots (`ax.bar()`, `ax.barh()`)
- Histograms (`ax.hist()`)
- Box plots (`ax.boxplot()`)
- Violin plots (`ax.violinplot()`)
- Error bars (`ax.errorbar()`)
- Filled plots (`ax.fill()`, `ax.fill_between()`)
- Contour plots (`ax.contour()`)
- Image plots (`ax.imshow()`)
- Seaborn plots (via integrated wrappers)

### `mngs.dsp` Module (Digital Signal Processing)

#### Signal Processing Functions

```python
# Filtering
filtered = mngs.dsp.filt.bandpass(signal, fs=1000, f_range=[8, 12])
filtered = mngs.dsp.filt.lowpass(signal, fs=1000, f_cutoff=30)

# Transforms
envelope = mngs.dsp.hilbert(signal, get='envelope')
phase = mngs.dsp.hilbert(signal, get='phase')
wavelet_output = mngs.dsp.wavelet(signal, fs=1000)

# Analysis
freqs, psd = mngs.dsp.psd(signal, fs=1000)
mi = mngs.dsp.pac(signal, fs=1000, f_phase=[2, 6], f_amp=[30, 90])
```

### `mngs.stats` Module

```python
# Format p-values with stars
stars = mngs.stats.p2stars(0.001)  # '***'

# Apply multiple comparison correction
corrected = mngs.stats.fdr_correction(results_df)

# Correlation tests
r, p = mngs.stats.tests.corr_test(x, y, method='pearson')
```

### `mngs.pd` Module (Pandas Utilities)

```python
# Round numeric values
rounded_df = mngs.pd.round(df, factor=3)

# Enhanced DataFrame slicing
filtered = mngs.pd.slice(df, {'column1': 'value', 'column2': [1, 2, 3]})

# Coordinate conversion
xyz_data = mngs.pd.to_xyz(df)
```

---

## Script Template

Every script should follow this standard format:

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "2024-11-03 10:33:13 (ywatanabe)"
# File: script_name.py

__file__ = "script_name.py"

"""
Functionalities:
  - Does XYZ
  - Saves XYZ

Dependencies:
  - scripts: /path/to/script1, /path/to/script2
  - packages: package1, package2

IO:
  - input-files: /path/to/input/file.xxx
  - output-files: /path/to/output/file.xxx
"""

"""Imports"""
import os
import sys
import argparse

"""Parameters"""
# from mngs.io import load_configs
# CONFIG = load_configs()

"""Functions & Classes"""
def main(args):
    # Main functionality goes here
    pass

def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    import mngs
    script_mode = mngs.gen.is_script()
    parser = argparse.ArgumentParser(description='')
    args = parser.parse_args()
    mngs.str.printc(args, c='yellow')
    return args

def run_main() -> None:
    """Initialize mngs framework, run main function, and cleanup."""
    global CONFIG, CC, sys, plt

    import sys
    import matplotlib.pyplot as plt
    import mngs

    args = parse_args()

    # Start mngs framework
    CONFIG, sys.stdout, sys.stderr, plt, CC = mngs.gen.start(
        sys,
        plt,
        args=args,
        file=__file__,
        sdir_suffix=None,
        verbose=False,
        agg=True,
    )

    # Main
    exit_status = main(args)

    # Close the mngs framework
    mngs.gen.close(
        CONFIG,
        verbose=False,
        notify=False,
        message="",
        exit_status=exit_status,
    )

if __name__ == '__main__':
    run_main()

# EOF
```

> **⚠️ DO NOT MODIFY THE `run_main()` FUNCTION**  
> This handles stdout/stderr direction, logging, configuration, and more

---

## Configuration Examples

### PATH.yaml

```yaml
# Time-stamp: "2025-01-18 00:00:34 (ywatanabe)"
# File: ./config/PATH.yaml

PATH:
  ECoG:
    f"./data/patient_{patient_id}/{date}/ecog_signal.npy"
```

### COLORS.yaml

```yaml
# Time-stamp: "2025-01-18 00:00:34 (ywatanabe)"
# File: ./config/COLORS.yaml

COLORS:
  SEIZURE_TYPE:
    "1": "red"
    "2": "orange"
    "3": "pink"
    "4": "gray"
```

Accessing configurations:

```python
import mngs
CONFIG = mngs.io.load_configs()

# Access config values
print(CONFIG.COLORS.SEIZURE_TYPE)  # {"1": "red", "2": "orange", "3": "pink", "4": "gray"}

# Resolve f-strings
patient_id = "001"
date = "2025_0101"
print(eval(CONFIG.PATH.ECoG))  # "./data/patient_001/2025_0101/ecog_signal.npy"
```

---

## Example: Plot with CSV Export

```python
import numpy as np
import mngs

# Create data
x = np.linspace(0, 10, 100)
y1 = np.sin(x)
y2 = np.cos(x)

# Create figure with mngs
fig, axes = mngs.plt.subplots(ncols=2, figsize=(10, 5))

# Plot data
axes[0].plot(x, y1, label='sin(x)')
axes[1].plot(x, y2, label='cos(x)')

# Use mngs wrapper methods for labels and titles
axes[0].set_xyt('x', 'y', 'Sine Function')
axes[1].set_xyt('x', 'y', 'Cosine Function')

# Add legends
for ax in axes:
    ax.legend()

# Save figure - CSV automatically exported with the same basename
mngs.io.save(fig, './data/figures/trig_functions.png', symlink_from_cwd=True)
```

This creates:
- `./data/figures/trig_functions.png` (the figure)
- `./data/figures/trig_functions.csv` (the data in CSV format)

---

## Example: Signal Processing

```python
import numpy as np
import mngs

# Generate test signal (10 Hz sine wave with 1000 Hz sampling rate)
fs = 1000  # Sampling frequency in Hz
t = np.arange(0, 1, 1/fs)  # 1 second of data
signal = np.sin(2 * np.pi * 10 * t)  # 10 Hz sine wave

# Apply bandpass filter
filtered_signal = mngs.dsp.filt.bandpass(signal, fs=fs, f_range=[8, 12])

# Calculate power spectral density
freqs, psd = mngs.dsp.psd(filtered_signal, fs=fs)

# Extract signal envelope
envelope = mngs.dsp.hilbert(filtered_signal, get='envelope')

# Plot results
fig, axes = mngs.plt.subplots(nrows=3, ncols=1, figsize=(10, 8))

# Plot signals
axes[0].plot(t[:500], signal[:500], label='Original')
axes[0].plot(t[:500], filtered_signal[:500], label='Filtered (8-12 Hz)')
axes[0].set_xyt('Time (s)', 'Amplitude', 'Time Domain')
axes[0].legend()

# Plot PSD
axes[1].plot(freqs, psd)
axes[1].set_xyt('Frequency (Hz)', 'Power/Frequency (dB/Hz)', 'Frequency Domain')
axes[1].set_xlim(0, 50)  # Display up to 50 Hz

# Plot envelope
axes[2].plot(t[:500], filtered_signal[:500], label='Filtered')
axes[2].plot(t[:500], envelope[:500], label='Envelope')
axes[2].set_xyt('Time (s)', 'Amplitude', 'Signal Envelope')
axes[2].legend()

# Save figure with automatic CSV export
mngs.io.save(fig, './data/figures/signal_analysis.png', symlink_from_cwd=True)
```

---

## Python Coding Style

### Naming Conventions

| Type | Convention | Example |
|------|------------|---------|
| Variable names | snake_case | `data_frame`, `patient_id` |
| Load paths | `lpath(s)` | `lpath = './data/input.csv'` |
| Save paths | `spath(s)` | `spath = './data/output.csv'` |
| Directory names | Noun form | `mnist-creation/`, `data-analysis/` |
| Script files | Verb first (for actions) | `classify_mnist.py`, `preprocess_data.py` |
| Class files | CapitalCase | `ClassName.py` |
| Constants | UPPERCASE | `MAX_ITERATIONS = 100` |

### Type Hints

```python
from typing import Union, Tuple, List, Dict, Any, Optional, Callable
from collections.abc import Iterable

# Define custom type aliases
ArrayLike = Union[List, Tuple, np.ndarray, pd.Series, pd.DataFrame, xr.DataArray, torch.Tensor]

def process_data(data: ArrayLike, factor: float = 1.0) -> np.ndarray:
    """Process the input data."""
    return np.array(data) * factor
```

### Docstring Format (NumPy Style)

```python
def func(arg1: int, arg2: str) -> bool:
    """Summary line.

    Extended description of function.

    Example
    ----------
    >>> xx, yy = 1, "test"
    >>> out = func(xx, yy)
    >>> print(out)
    True

    Parameters
    ----------
    arg1 : int
        Description of arg1
    arg2 : str
        Description of arg2

    Returns
    -------
    bool
        Description of return value
    """
    return True
```

For simple functions, one-line docstrings are acceptable:

```python
def add(a: int, b: int) -> int:
    """Add two numbers and return the result."""
    return a + b
```

### Function Organization

Organize functions hierarchically:

```python
# 1. Main entry point
# ---------------------------------------- 
def main():
    """Main function that coordinates the workflow."""
    data = load_data()
    processed = process_data(data)
    save_results(processed)


# 2. Core functions
# ---------------------------------------- 
def load_data():
    """Load and prepare input data."""
    pass

def process_data(data):
    """Process the data."""
    pass

def save_results(results):
    """Save processing results."""
    pass


# 3. Helper functions
# ---------------------------------------- 
def validate_inputs(data):
    """Validate input data."""
    pass
```

---

## Testing Guidelines

- Use pytest (not unittest)
- Create small, focused test functions
- One test function per test file
- Define test classes in dedicated scripts

### Test File Structure

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-03 00:49:28 (ywatanabe)"
# File: ./tests/mngs/plt/test_function.py

import pytest
import numpy as np

def test_function():
    """Test specific functionality."""
    from mngs.module.path import function
    
    # Setup test data
    input_data = np.array([1, 2, 3])
    
    # Call function
    result = function(input_data)
    
    # Assert expected results
    assert isinstance(result, np.ndarray)
    assert result.shape == (3,)
    assert np.array_equal(result, np.array([2, 4, 6]))

if __name__ == "__main__":
    import os
    pytest.main([os.path.abspath(__file__)])
```

### Running Tests

Use the script in the project root:

```bash
./run_tests.sh
```

---

## Statistical Reporting

Report statistical results with:
- p-value
- Significance stars
- Sample size
- Effect size
- Test name
- Statistic value
- Null hypothesis

```python
# Example results dictionary
results = {
    "p_value": pval,
    "stars": mngs.stats.p2stars(pval),  # Format: 0.02 -> "*", 0.009 -> "**"
    "n1": n1,
    "n2": n2,
    "dof": dof,
    "effsize": effect_size,
    "test_name": test_name_text,
    "statistic": statistic_value,
    "H0": null_hypothesis_text,
}
```

### Using p2stars

```python
>>> mngs.stats.p2stars(0.0005)
'***'
>>> mngs.stats.p2stars("0.03")
'*'
>>> mngs.stats.p2stars("1e-4")
'***'
>>> df = pd.DataFrame({'p_value': [0.001, "0.03", 0.1, "NA"]})
>>> mngs.stats.p2stars(df)
   p_value
0  0.001 ***
1  0.030   *
2  0.100
3     NA  NA
```

### Multiple Comparisons Correction

Always use FDR correction for multiple comparisons:

```python
# Apply FDR correction to DataFrame with p_value column
corrected_results = mngs.stats.fdr_correction(results_df)
```

---

## Path Management

- Use relative paths from project root
- Start paths with `./` or `../`
- Execute scripts from project root
- Use symlinks for data organization
- Centralize path definitions in `./config/PATH.yaml`

For mngs package development:
- Use underscore prefix for imports (e.g., `import numpy as _np`)
- Use relative imports (e.g., `from ..io._load import load`)

---

CLAUDE UNDERSTOOD: /data/gpfs/projects/punim2354/ywatanabe/mngs_repo/docs/mngs_guidelines.md

<!-- EOF -->