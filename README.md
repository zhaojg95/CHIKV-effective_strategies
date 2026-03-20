# CHIKV-effective_strategies
Effective strategies to control the largest chikungunya fever outbreak in China

The projecet primary analyzes the effective strategies to control the largest chikungunya fever outbreak in China in 2025, including two models:  City-level Chikungunya Transmission Model (the code in CCTM file), and Multi-district Chikungunya Transmission Model (the code in MCTM file). In CCTM and MCTM files, all include three main folders: (1) data, (2) code, and (3) result.

### CCTM file
In the first folder (named "1.data"), it contains the cases data required for the manuscript (named Foshan_cases.xlsx). The first sheet is the data for simulation fitting (Sheet1), and the second sheet is the data for model validation (Sheet2). The second data was the MOI data during 2018-2023, which is used to validate the model’s performance in simulating mosquito population dynamics.

In the code folder (named "2.code"), it contains the following:

#### 1.Data fitting and plot CF trend(named "1.fitted_multi-chian.R")
#### 2.R0 calculation ("2.r0.R") 
#### 3.Simulate the trends under different types of interventions and at different timings ("3.simulate_foshan_combind_measures+timing.R")
#### 4. Plot the results of different scenarios ("4. plot_scenarios_result.R")
#### 5. Population dynamics of each compartment under the simulated baseline scenario ("5. population dynamics of all compartments.R")
#### 6.1 Validation of model predictions under intervention scenarios("validation_combind_measures_result.R")
#### 6.2 validation of the model’s performance in simulating mosquito population dynamics ("6.2 validation_MOI&Nm.R")
#### 7.Evaluate the actual proportion of mosquitoes removed each day ("7. actual daily removal proportions.R")
#### 8.Sensitivity analysis ("8. Sensitivity analysis -- parameter & nitial value.R")

The code 'CHIKV-model.R' contains the baseline model 'CHIKV_model' and the intervention model 'CHIKV_model_with_combind_intervention'. The codes 'fixed parameters.R' and 'init_value & fixed parameters.R' contain the model's fixed parameters and some initial values. The codes 'plot function.R' and 'Fig 5 & Fig S13.ipynb' are used to store the code for plotting the model results. The code 'result_sim.R' is used to summarize the results.
The results file is used to store the results of data analysis and visualization.


### MCTM file
In this file, in the first folder (named "1.data"), it contains the data required for MCTM, including "address.xlsx" (which records the addresses and latitude/longitude of all districts in Foshan),  "Foshan_cases.xslx"(wihch same as the data in model CCTM), "Foshan_cases_byDistrict.xlsx" (this data recoreds the CF cases of different districts), "Foshan-baidu migration index.xlsx" (the baidu migration index in Foshan), "OD(fs).xlsx" (records the distance between different districts)

In the code folder (named "2.code"), it contains the following:
#### 0.Generate OD matrix based on longitude and latitude ("0. Calculate OD.R")
#### 1.Data fitting (named "1.fitting_MCTM.R")
#### 2.Simulate the trends under baseline scenario in MCTM("2.simulate_foshan_combinded_measures_meta population.R")
#### 3.Simulate the trends under different types of interventions ("3.simulate_foshan_combinded_measures_meta population_intervention.R")

The code 'Metapopulation_model.R' contains the baseline model 'Foshan_Meta_Model' and the intervention model 'Foshan_Meta_Model_intervention'.  The code 'init_meta_parameters.R' contain the model's parameters.

The R version is available on CRAN https://cran.r-project.org/mirrors.html. The Python version is available at https://www.python.org/downloads/.
