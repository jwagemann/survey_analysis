# User requirements of Big Earth Data - Survey 2019

This repo hosts the code developed to analyse and visualise the responses from a user requirements survey. The programming language R was used for data analysis and visualisation.

## User requirements of Big Earth Data
The survey was conducted between November 2018 and May 2019 with the aim to find out how users working with large volumes of environmental data interact with data, what challenges they face and how they would like to use cloud-based data services in the future.

The term `Big Earth Data` in this context refers to *digital information about Earth, including observations, imagery, derived higher-level products, forecasts and analyses produced by computer models*.

The survey was conducted in collaboration with the [European Centre for Medium-Range Weather Forecasts (ECMWF)](https://www.ecmwf.int) and as part of a PhD thesis on "Big Data technologies for environmental and climate data" at University of Marburg, Germany.

The questionnaire and an anonymised version of the survey responses are available on Zenodo with the following DOI:
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4075058.svg)](https://doi.org/10.5281/zenodo.4075058).

Publication of the results is currently under review in the journal [Computer & Geosciences](https://www.journals.elsevier.com/computers-and-geosciences).

## Repository structure
The repository is structured based on the order of the survey questions. There are two base modules, who load the data, required libraries and define some functions:
- [data_load.R](./data_load.R)
- [data_survey_functions.R](./data_survey_functions.R)

Both scripts have to be loaded before the other scripts can be run. The other scripts have the code to analyse and visualise the respecitve question of the survey questionnaire.



## License
<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License</a>.
