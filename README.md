# CHIKV-effective_strategies
## Effective strategies to control a large chikungunya fever outbreak in China

## Overview
This project analyses effective strategies for controlling the chikungunya fever (CHIKF) outbreak in China in 2025. It contains two models:
City-level Chikungunya Transmission Model (CCTM) — code in the CCTM folder
Multi-district Chikungunya Transmission Model (MCTM) — code in the MCTM folder
Each model folder contains three subfolders: 1.data, 2.code, and 3.result.
### CCTM folder

#### 1. Data
**1.data** contains the data required for the manuscript:
Foshan_cases.xlsx — Sheet1: data for simulation fitting; Sheet2: data for model validation.
MOI data (2018–2023) — used to validate the model's performance in simulating mosquito population dynamics.

#### 2. Code
**2.code** contains the analysis scripts. Run the scripts in the following order:

1.**fitted_multi-chain.R** — Data fitting and plotting of the CF trend.

2.**r0_value.R** — Calculation of R₀.

3.**simulate_foshan_combind_measures+timing.R** — Simulation of trends under different intervention types and timings.

4.**plot_scenarios_result.R** — Plotting of results for different scenarios.

5.**population dynamics of all compartments.R** — Population dynamics of each compartment under the simulated baseline scenario.

6.1 **validation_combind_measures_result.R**— Validation of model predictions under intervention scenarios.

6.2 **validation_MOI&Nm.R** — Validation of the model's performance in simulating mosquito population dynamics.

7.**actual daily removal proportions.R** — Evaluation of the actual proportion of mosquitoes removed each day.

8.**Sensitivity analysis -- parameter & initial value.R** — Sensitivity analysis.

Supporting scripts:
**CHIKV-model.R** — Baseline model (CHIKV_model) and intervention model (CHIKV_model_with_combind_intervention).

**fixed parameters.R** and **init_value & fixed parameters.R** — Fixed parameters and initial values.

**plot function.R** and **Fig 5 & Fig S13.ipynb** — Plotting code for model results.

**result_sim.R** — Summarises the results.

#### 3. Result
**3.result** stores the results of data analysis and visualization.

### MCTM folder
#### 1. Data
**1.data** contains the data required for MCTM:
address.xlsx — Addresses and latitude/longitude of all districts in Foshan.
Foshan_cases.xlsx — Same as the data in CCTM.
Foshan_cases_byDistrict.xlsx — CHIKF cases in different districts.
Foshan-baidu migration index.xlsx — Baidu migration index in Foshan.
OD(fs).xlsx — Distance between different districts.

#### 2. Code
**2.code** contains the analysis scripts. Run the scripts in the following order:

**0.Calculate OD.R** — Generate the OD matrix based on longitude and latitude.

**1.fitting_MCTM.R** — Data fitting.

**2.simulate_foshan_combinded_measures_meta population.R** — Simulation of trends under the baseline scenario in MCTM.

**3.simulate_foshan_combinded_measures_meta population_intervention.R** — Simulation of trends under different types of interventions.

Supporting scripts:
**Metapopulation_model.R** — Baseline model (Foshan_Meta_Model) and intervention model (Foshan_Meta_Model_intervention).
**init_meta_parameters.R** — Model parameters.

#### Software requirements
R (available on CRAN: https://cran.r-project.org/mirrors.html)
Python (available at: https://www.python.org/downloads/)

#### Data availability
The datasets generated and/or analysed in this study are available from Zenodo (https://doi.org/10.5281/zenodo.21413279)

#### Authorization
如需使用本项目代码或数据，请先联系原作者获得授权。
Please contact the original author for authorization before using the code or data.
Users are expected to maintain strong data security and privacy protection awareness. If you publish research or results based on this project, please properly cite the original source.

Contact: zhao_jg95@163.com

