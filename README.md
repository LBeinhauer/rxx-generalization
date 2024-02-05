# Erroneous Generalization - Exploring Random Error Variance in Reliability Generalizations of Psychological Measurements

This is git repository associated with the preprint located at [osf.io/ud9rb](https://osf.io/ud9rb) (PsyArXiv). The project proposes a novel approach to Reliability Generalizations which have the goal of assessing differences in measuring quality of psychological instruments across administrations. The general idea is that, under CTT, score reliability is a function of true and error score variance. As the error score variance pertains the measuring quality, differences in score reliability are not necessarily indicative for impairment of measuring quality across some administrations. Instead, we propose that Reliability Generalization studies may be enriched by explicit models of error score variance. More details can be found at [osf.io/ud9rb](https://osf.io/ud9rb).

Within this repository, scripts for the simulation scheme and its analysis are documented and provided, enabling a reproduction of results in the preprint. Similarly, scripts to manipulate and assess the empirical data used for demonstrations are provided. Since the empirical data stems from public repositories, we do not provide any data, but scripts to fetch the data from their respective repositories.

## Getting Started

Running through the scripts in order allows for a reproducting of results discussed in the preprint.
Scripts starting with an "S" are used in the simulation, while scripts starting with an "E" are used in the analysis of empirical data.

To reproduce the simulation results, run through the scripts [S1](S1_Simulation_future.R), [S2](S2_Extraction.R), [S3](S3_Aggregation.R), and [S4](S4_Graphics.R) in the correct order.

To reproduce the results from the analysis of empirical data, run through the scripts [E0](E0_Download_Data.R), [E1](E1_Data_Extraction.R), [E2](E2_Estimating_Reliability.R), [E3](E3_Reliability_RE-MA.R), and [E4](E4_Variances_RE-MA.R), also in correct order.

Additionally, simulation results may be explored by the [shiny app](Shiny_Simulation_Exploration.R).

### Short overview of scripts

- [RG_function_library.R](RG_function_library.R) is a simple script containing all self-defined functions which are used throughout the remaining R- scripts. Other scripts call upon this file.

- [S1_Simulation_future.R](S1_Simulation_future.R) is a script to run a large-scale simulation scheme, assessing some features of explicitly modelling error score variance.

- [S2_Extraction.R](S2_Extraction.R) is a script to extract the simulated data and subject it to some inital data-manipulation.

- [S3_Aggregation.R](S3_Aggregation.R) is a script to aggregate and back-transform the simulated data to its original scale.

- [S4_Graphics.R](S4_Graphics.R) is a script to prepare the graphics found in the pre-print. It also prepares the data used in the [Shiny-App](Shiny_Simulation_Exploration.R).

- [E0_Download_Data.R](E0_Download_Data.R) is a script to download the empirical data. The public repositories were data is used from are: [Many Labs 3](https://osf.io/ct89g/?view_only=) (Ebersole et al. 2016); [Many Labs 5 - Shnabel & Nadler](https://osf.io/kz6q5/) (Baranski et al. 2020); [Registered Replication Report - Finkel et al.](https://osf.io/s3hfr/) (Cheung et al. 2016); [Registered Replication Report - Mazar/Srull](https://osf.io/vxz7q/) (Verschuere et al. 2018, McCarthy et al. 2018)

- [E1_Data_Extraction.R](E1_Data_Extraction.R) is a script to extract the data from the downloaded data-files. It references the "extraction scripts" in [Extraction_Scripts](Extraction_Scripts).

- [E2_Estimating_Reliability.R](E2_Estimating_Reliability.R) is a script to estimate score reliability for each replication/administration of the assessed scales.

- [E3_Reliability_RE-MA.R](E3_Reliability_RE-MA.R) is a script to perform standard reliability generalization analyses, using random-effects meta-analyses and Bonett-transformations.

- [E4_Variances_RE-MA.R](E4_Variances_RE-MA.R) is a script to perform random-effects meta-analytic models on estimates of error score variance.

- [Shiny_Simulation_Exploration.R](Shiny_Simulation_Exploration.R) is a R-shiny-app, providing some visualisations to make exploration of simulation results more accessible.

### Prerequisites

Users should have a (somewhat) recent version of R installed.
Analyses in E1:E4 and S2:S4 were performed in R version 4.3.1. Simulation in S1 was performed using version 4.2.2 (on a remote server).

