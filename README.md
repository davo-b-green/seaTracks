# seaTracks

seaTracks simplifies the process of accessing, compiling, and processing satellite telemetry data from the Sea Mammal Research Unit (SMRU) Data Portal. It provides streamlined functions for downloading SRDL data, compiling datasets from multiple campaigns, and interfacing with analytical packages to fit state-space models and move persistence models.

## Features
- **Data Downloading**: Directly access data from the SMRU portal.
- **File Management**: Automated file handling with default settings for storage paths.
- **Data Compilation**: Efficiently compile diagnostic, dive, ctd, haulout, and summary datasets.
- **Data Analysis**: Interface with the `aniMotum` package to fit state space and move persistence models on animal location data.

## Installation

Install the development version from GitHub:

```r
# Install the devtools package if not already installed
# install.packages("devtools")
devtools::install_github("davo-b-green/seaTracks")
```
## Usage
The package is structured to be used in a sequential workflow from data download to analysis:

### Data Downloading:

1. **get_SMRU_files()**: Download SRDL files from the SMRU Data Portal.
Data Compilation:

2. **compile_diag()**: Compile diagnostic location datasets.
3. **compile_dive()**: Compile dive datasets.
- **compile_ctd()**: Compile CTD (Conductivity, Temperature, Depth) datasets.
- **compile_haul()**: Compile haulout datasets.
- **compile_sum()**: Compile summary datasets.

### Data Processing:

- **process_tracks()**: Process the compiled datasets using aniMotum's state-space model.
  
- **compute_dive_metrics()**: Calculate detailed foraging metrics from dive data. This is primarily intended for southern elephant seals.

### Example
Here is a basic example which demonstrates setting up a workflow:

```{r}
library(seaTracks)
# Example of downloading data
get_SMRU_files(dir = "./data", campaigns = "ct100", providers = "imos")

# Compile location data
compiled_diag <- compile_diag(acc_path = "./data/access_files", dir = "./data/compiled")
```
## Dependencies
seaTracks is a convenience pacakge that is entirely dependent on two much more powerful R packages:

- **rSRDL**: Access and QC raw diag, dive, CTD, haulout, and summary tables.
- **aniMotum**: Fit state-space models and move persistence models.

Please ensure these are installed and loaded as required.

## Contributing
Contributions to seaTracks are welcome via pull requests and issues on the GitHub repository.

## License
This package is available under the MIT License. Full license details can be found in the LICENSE file.

## Authors
David Green - Initial work - ORCID

## Acknowledgements
This package builds upon the work and tools provided by the rSRDL and aniMotum packages. Please cite these packages appropriately when using seaTracks for your research.
