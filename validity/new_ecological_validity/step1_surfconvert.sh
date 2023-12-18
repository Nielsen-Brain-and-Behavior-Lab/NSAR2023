#!/bin/bash

#Purpose: Convert 32k fs_LR surface to fsaverage6 surface
#Inputs: HCP COPE4 STORY-MATH task activation zstats files in 32k fs_LR space.
#Outputs: HCP task activation tstats files in fsaverage6 space.
#Software: wb_command (HCP), freesurfer
#32k and fsaverage templates: https://github.com/Washington-University/HCPpipelines/tree/master/global/templates/standard_mesh_atlases/resample_fsaverage
#Instructions: https://www.bing.com/ck/a?!&&p=e6ca950a079e447fJmltdHM9MTY3NDY5MTIwMCZpZ3VpZD0xMzRiMmFhYi04ZWYwLTY3NTMtMmJhZi0zOGMzOGYzNzY2OTkmaW5zaWQ9NTE3Nw&ptn=3&hsh=3&fclid=134b2aab-8ef0-6753-2baf-38c38f376699&psq=resample+freesurfer+hcp&u=a1aHR0cHM6Ly93aWtpLmh1bWFuY29ubmVjdG9tZS5vcmcvZG93bmxvYWQvYXR0YWNobWVudHMvNjMwNzg1MTMvUmVzYW1wbGluZy1GcmVlU3VyZmVyLUhDUC5wZGY&ntb=1 (see part C)
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2023

#Load workbench via sourcing CBIG config file
source /fslgroup/fslg_rdoc/compute/test_scripts/parc_scripts/CBIG_preproc_tested_config_funconn.sh

#PATHS
CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/HCP_analysis/lang_lat/lana_mask
DATA_DIR=/fslgroup/grp_proc/compute/HCP_analysis/HCP_download
FSLR_DIR=/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/fs_LR_32k
FSAVERAGE_DIR=/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/fsaverage6

#LOOP
for sub in `ls $DATA_DIR`; do 
	#1. Cifti-separate the RAW tstats file
	wb_command -cifti-separate $DATA_DIR/${sub}/tstat1.dtseries.nii COLUMN -metric CORTEX_LEFT ${DATA_DIR}/${sub}/sub-${sub}_tstat1_LH.shape.gii -metric CORTEX_RIGHT ${DATA_DIR}/${sub}/sub-${sub}_tstat1_RH.shape.gii

	#2. Map metric data (32k to fsaverage6)
	#RH
	wb_command -metric-resample ${DATA_DIR}/${sub}/sub-${sub}_tstat1_RH.shape.gii ${FSLR_DIR}/fs_LR-deformed_to-fsaverage.R.sphere.32k_fs_LR.surf.gii ${FSAVERAGE_DIR}/fsaverage6_std_sphere.R.41k_fsavg_R.surf.gii ADAP_BARY_AREA ${DATA_DIR}/${sub}/sub-${sub}_tstat1_R_fsaverage6.func.gii -area-metrics ${FSLR_DIR}/fs_LR.R.midthickness_va_avg.32k_fs_LR.shape.gii ${FSAVERAGE_DIR}/fsaverage6.R.midthickness_va_avg.41k_fsavg_R.shape.gii
	#LH
	wb_command -metric-resample ${DATA_DIR}/${sub}/sub-${sub}_tstat1_LH.shape.gii ${FSLR_DIR}/fs_LR-deformed_to-fsaverage.L.sphere.32k_fs_LR.surf.gii ${FSAVERAGE_DIR}/fsaverage6_std_sphere.L.41k_fsavg_L.surf.gii ADAP_BARY_AREA ${DATA_DIR}/${sub}/sub-${sub}_tstat1_L_fsaverage6.func.gii -area-metrics ${FSLR_DIR}/fs_LR.L.midthickness_va_avg.32k_fs_LR.shape.gii ${FSAVERAGE_DIR}/fsaverage6.L.midthickness_va_avg.41k_fsavg_L.shape.gii
done 


