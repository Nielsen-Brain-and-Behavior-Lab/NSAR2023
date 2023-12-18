# New Ecological Validity Analysis

## Description

This is the ecological validity analysis that appears in the preprint, replacing the weaker ecological validity evidence from comparing language network laterality against language laterality from the LanA atlas.

Here, we performed an analysis comparing language network laterality against language task activation laterality in HCP subjects. We used minimally preprocessed language task activation t-statistic files downloaded from the HCP database connectomedb.

The LanA atlas can be downloaded from http://evlabwebapps.mit.edu:8763/#/download

## Contents

1. fsaverage6_conversion.sh This script is used to convert the LanA atlas to fsaverage6 resolution (to facilitate comparison with the group parcellation)
2. ?H_LanA_n804.shape.gii LanA atlas GIFTI files in fsaverage6 resolution
3. lang_subs.txt Contains subject list of participants with available t-statistic files.
4. step1_surfconvert.sh Converts 32k fs_LR surface to fsaverage6 surface
5. mask_and_lat.m Uses the LanA atlas to mask the t-statistics files and then calculate laterality (outputted in a .csv file)
