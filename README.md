
# seaTracks

<!-- badges: start -->
<!-- badges: end -->

seaTracks is designed to simplify the process of accessing, compiling and processing satellite telemetry data available through the Sea Mammal Research Unit Data Portal. Its key functions are to access the online repository, download SRDL data, compile datasets from multiple campaigns, and then to interface with other analytical packages to process the data by fitting state space models to estimate locations at a regularised time interval, as well as for all summarised dive and ctd profiles.

One of the main advantages of seaTracks is that it makes file and dataset management easier, because if has default settings on where to store and expect files to be located. Running the functions on default settings allows you to not think about where files should be written or even what to call them.



Data preparation and processing relies heavily on the packages:

- [rSRDL](https://github.com/embiuw/rSRDL/tree/master) for accessing and qc'ing the raw diag, dive, ctd, haulout and summary tables from the available Microsoft Access files
- [aniMotum](https://github.com/ianjonsen/aniMotum) for fitting state-space models and move persistence models on the compiled datasets

Before continuing with the workflow presented here, I strongly encourage that you read the descriptions of these packages to understand how they process the data. Please also appropriately cite these packages in any work produced.



## Installation

You can install the development version of seaTracks from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("davo-b-green/seaTracks")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(seaTracks)
## basic example code
```

