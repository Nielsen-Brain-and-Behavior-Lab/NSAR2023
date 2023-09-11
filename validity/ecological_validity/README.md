# Ecological Validity Analysis (HCP dataset)

## Description

For this analysis, we compared the LanA language atlas against the language network from the HCP (N=232) group parcellation. The LanA atlas can be downloaded from http://evlabwebapps.mit.edu:8763/#/download

## Contents

1. fsaverage6_conversion.sh This script is used to convert the LanA atlas to fsaverage6 resolution (to facilitate comparison with the group parcellation)
2. ?H_LanA_n804.shape.gii LanA atlas GIFTI files in fsaverage6 resolution
3. parc_step_1.m Code used to generate HCP group parcellation
4. Hungarian_matching_group.m Hungarian match the HCP group parcellation
5. parc_2_annot_group.m Convert group parcellation from .mat to annotation file
6. convert_annot2gii_group.sh Convert annotation file to GIFTI format
