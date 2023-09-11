# Autonomy Index (HCP dataset)

## Description
The scripts in the folder are used to calculate the autonomy index on each vertex of a subject-averaged functional connectivity matrix. Then, autonomy index values are averaged within network boundaries (separate for the right and left hemispheres). Individual parcellations are used to delineate network boundaries (MS-HBM Kong2019). 

## Contents
1. ai_ALL_wrapper.sh Wrapper script used to submit jobs for each subject
2. ai_ALL_job.sh Job script
3. ai_ALL_SINGLE.m MATLAB template script that is updated for each subject
4. subjids.txt Text file containing anonymized subject IDs (HCP 232)
5. ai2gii_ALL.m Converts AI .mat files to GIFTI format for HCP Workbench viewing
6. INDIVIDUAL_avg_ai_MSHBM_HCP_ALL.py Python (v3.6) script used to average autonomy index values within boundaries of individual parcellations
