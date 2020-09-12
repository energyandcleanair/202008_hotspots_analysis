# 2020 SO2 Hotspots

The project works in two steps:
- download and extract OMI values around locations of interest (`extract_omi_data()` function)
- build statistical models between NASA MEASURES data and OMI concentration data

## How it works
Simply run `run.R`. It should create various experiment subfolders in the `results` folder.

Experiments are named with the following nomenclature:
- `source_` : means one interpolation per source is made
- `_ci`: 'confidence' interval is used for model uncertainty
- `_pi`: 'prediction' interval is used for model uncertainty

## Uncertainty calculations
- [Methodology](doc/uncertainty.pdf)
