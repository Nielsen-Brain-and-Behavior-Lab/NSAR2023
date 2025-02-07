# Brain Network Lateralization through NSAR 2023

## Purpose
The aim of this study was to estimate network lateralization using a network surface area-based approach (NSAR). After examining the validity and reliability of individual network parcellations and this surface area-based measure of lateralization, we addressed two hypotheses. First, we hypothesized that networks associated with language, visuospatial attention, and executive control would show the greatest lateralization. Second, we hypothesized that network lateralization would exhibit a dependent relationship between lateralized networks across individuals.

## Associated Publications 
* *Imaging Neuroscience* Publication: https://doi.org/10.1162/imag_a_00437
* Preprint: https://doi.org/10.1101/2023.12.08.570817

## Getting Started 
Scripts are organized in the following folders: preproc, ind_parc, network_sa, validity, reliability, and stats.

* The *preproc* folder contains scripts for data organization, FreeSurfer, resting-state preprocessing (the CBIG2016 pipeline), and tSNR calculation.
* The *ind_parc* folder contains scripts for running the Kong2019 MS-HBM pipeline and visualization.
* The *network_sa* folder contains scripts for calculating network surface area using workbench_command.
* The *validity* and *reliability* folders contain scripts used to run various validity and reliability analyses.
* The *stats* folder contains scripts for manuscript figure generation and statistical analyses.

README files can be found in each folder, so please see those for additional details.

## Tutorial Guide 
Step-by-step walkthroughs are avaialble for the preprocessing and MS-HBM parcellation steps on [NeuroDocs](https://neurodocs.readthedocs.io/en/latest/).

## Contact
For questions concerning script usage, please contact us through our lab webpage: https://brain.byu.edu/contact. Any questions regarding the usage of software/pipelines developed by other labs (e.g., fMRIprep) should be directed to their respective forums. Best of luck!
