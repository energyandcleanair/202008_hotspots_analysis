# 2020 SO2 Hotspots

The project works in two steps:
- download and extract OMI values around locations of interest (`extract_omi_data()` function)
- build statistical models between NASA MEASURES data and OMI concentration data

## Results
Data:
- [Year-on-year predictions (RDS)](results/data/omi_predictions.RDS)
- [Year-on-year predictions (csv)](results/data/omi_predictions.csv)

![predictions_zoom](results/plots/prediction_2020_zoom.png?raw=true)
![prediction_quality](results/plots/pearson_sourcetype.png?raw=true)
