#!/bin/bash

#Purpose: Create Conte69 midthickness surface file in fsaverage6 space using Workbench.
#Inputs: Conte69 midthickness fs_LR 32k (download: https://raw.githubusercontent.com/MidnightScanClub/MSCcodebase/master/Utilities/Conte69_atlas-v2.LR.32k_fs_LR.wb/Conte69.L.midthickness.32k_fs_LR.surf.gii)
#Outputs: Conte69 midthickness .surf.gii files in fsaverage6 resolution.
#Templates: Paths and download links are found in step1 of this same folder.
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2023.

#Load workbench via sourcing CBIG config file
source /fslgroup/fslg_rdoc/compute/test_scripts/parc_scripts/CBIG_preproc_tested_config_funconn.sh

#PATHS
CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/HCP_analysis/lang_lat
DATA_DIR=/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/conte69
FSLR_DIR=/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/fs_LR_32k
FSAVERAGE_DIR=/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/fsaverage6


#RESAMPLE
	#RH
	wb_command -surface-resample ${DATA_DIR}/Conte69.R.midthickness.32k_fs_LR.surf.gii ${FSLR_DIR}/fs_LR-deformed_to-fsaverage.R.sphere.32k_fs_LR.surf.gii ${FSAVERAGE_DIR}/fsaverage6_std_sphere.R.41k_fsavg_R.surf.gii BARYCENTRIC ${DATA_DIR}/Conte69_midthickness_fs6_RH.surf.gii
	#LH
	wb_command -surface-resample ${DATA_DIR}/Conte69.L.midthickness.32k_fs_LR.surf.gii ${FSLR_DIR}/fs_LR-deformed_to-fsaverage.L.sphere.32k_fs_LR.surf.gii ${FSAVERAGE_DIR}/fsaverage6_std_sphere.L.41k_fsavg_L.surf.gii BARYCENTRIC ${DATA_DIR}/Conte69_midthickness_fs6_LH.surf.gii


	#Convert to .shape.gii
	#RH
	wb_command -surface-vertex-areas ${DATA_DIR}/Conte69_midthickness_fs6_RH.surf.gii ${DATA_DIR}/Conte69_midthickness_fs6_RH.shape.gii
	#LH
	wb_command -surface-vertex-areas ${DATA_DIR}/Conte69_midthickness_fs6_LH.surf.gii ${DATA_DIR}/Conte69_midthickness_fs6_LH.shape.gii



