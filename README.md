2022 Ebola Outbreak (Uganda) <img src='www/logo.svg' align='right' alt='' width='200' />
====================================================================================================

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->



Context
----------------------------------------------------------------------------------------------------
This code aims to analyze and visualize the 2022 Ebola outbreak in Uganda in collaboration with the Ministry of Health.


TODOs
----------------------------------------------------------------------------------------------------
## Data Loading and Validation

- [ ] id relevant data and load in
    - [ ] cases
    - [ ] relationships
    - [x] linelist - national
    - [ ] linelist - entebe
- [ ] validate datasets - as needed

## Analysis : Descriptive

- [x] epicurve x outcome + survival %
- [x] epicurve x HCW
- [x] HCW % x geo
- [x] sex/age pyramid
- [ ] age over time heatmap
- [x] kernel densitty t to isolation x outocme
- [ ] symptoms
    - [ ] RR confirmation | symp
    - [ ] % x symp | conf
    - [ ] % x symp | !conf

## Analysis : Spatial
- [ ] basic map of cases -- choropleth?
- [ ] basic map with bubbles
- [ ] movement flows
- [ ] transmission trees in space

## Analysis : Transmission Trees

- [x] basic tree
- [ ] inferred links?
- [ ] stats
    - [ ] generations
    - [ ] size
    - [ ] secondaries

