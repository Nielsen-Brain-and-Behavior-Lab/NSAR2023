# HCPD Network Overlap Figure

## Contents
1. step1_generate_network_overlap.m A MATLAB script that computes the network overlap for each of 17 networks 
2. step2_overlap2gii.m A MATLAB script that generates GIFTI files for the LH and RH for each of the 17 networks (essentially brain maps of the network overlap calculated in step 1)
3. ids text files Two text files that contain the anonymized subject IDs for the HCPD dataset

## Additional Notes
These scripts were optimized for MATLAB version r2018b. 

It is assumed that individual network parcellations have previously been generated for each subject, and that the networks have been Hungarian-matched. 
