# New Ecological Validity Analysis

## Description
This is the ecological validity analysis that appears in the preprint, replacing the weaker ecological validity evidence from comparing language network laterality against language laterality from the LanA atlas.

Here, we performed an analysis comparing language network laterality against language task activation laterality in HCP subjects. We used minimally preprocessed language task activation t-statistic files downloaded from the HCP database connectomedb.

## Contents
1. lang_subs.txt Contains subject list of participants with available t-statistic files.
2. step1_surfconvert.sh Converts 32k fs_LR surface to fsaverage6 surface
3. mask_and_lat.m Uses the LanA atlas to mask the t-statistics files and then calculate laterality (outputted in a .csv file)
