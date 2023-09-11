# Stable Estimate Analysis (HCP dataset)

## Description
Given that MRI scanning is costly, rendering it comparatively rare to have highly sampled individuals, it is important to understand how much data is needed to reliably estimate specialization. 

To address this concern, we we utilized HCP participants with all four runs available (N = 232). Following preprocessing, the first and third scans from each participant were set aside to compose 30 minutes of independent data. Next, the second and fourth scans were each split into three five-minute segments. The MS-HBM pipeline was then used to generate individual parcellations from 5, 10, 15, 20, 25, and 30 minutes of data from the segmented scans. The MS-HBM pipeline was also used to generate separate individual parcellations from 30 minutes of independent data. 

## Contents
1. Split runs: split_surf_runs_fs.m
2. Example dice coefficient script: HCP_5MIN_2SESS_dice.py 

## Additional Notes
The MS-HBM parcellation scripts and NSAR scripts used for each iteration (5 min, 10 min, 15 min, etc.) are not included here due to redundancy. Instead, the reader is directed to the ind_parc/HCP and network_SA/HCP directories for the highly similar code used to perform these functions.
