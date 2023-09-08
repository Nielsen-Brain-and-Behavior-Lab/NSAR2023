#Purpose: Calculate SA per network using fsaverage6 resolution surfaces
#Inputs: Kong2019 parc parc2annot and annot2gii output. Midthickness Conte69 in fsaverage6 space.
#Output: Surface area for LH and RH for 17 networks 
#Software: wb_command (HCP), freesurfer
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2023

#Load workbench via sourcing CBIG config file
source /fslgroup/fslg_rdoc/compute/test_scripts/parc_scripts/CBIG_preproc_tested_config_funconn.sh

#Load freesurfer
export FREESURFER_HOME=/fslhome/mpeter55/compute/research_bin/freesurfer
source $FREESURFER_HOME/SetUpFreeSurfer.sh

#PATHS
CODE_DIR=/fslgroup/fslg_spec_networks/compute/code/HCPD_analysis/network_sa/REST
DATA_DIR=/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/quant_metrics/MSHBM_vis
CONTE_DIR=/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces/conte69

#LOOP
for sub in `cat ${CODE_DIR}/subjids/REST_ids.txt`; do 
	for network in {0..17}; do
	#Extract label as ROI
	#LH
	wb_command -gifti-label-to-roi ${DATA_DIR}/sub-${sub}_lh.label.gii ${DATA_DIR}/sub-${sub}_LABEL${network}_LH.shape.gii -key $network
	#RH
	wb_command -gifti-label-to-roi ${DATA_DIR}/sub-${sub}_rh.label.gii ${DATA_DIR}/sub-${sub}_LABEL${network}_RH.shape.gii -key $network

	#Calculate surface area on midthickness.shape.gii
	#LH
	LH_SA=`wb_command -metric-stats ${CONTE_DIR}/Conte69_midthickness_fs6_LH.shape.gii -reduce SUM -roi ${DATA_DIR}/sub-${sub}_LABEL${network}_LH.shape.gii`
  	#RH
	RH_SA=`wb_command -metric-stats ${CONTE_DIR}/Conte69_midthickness_fs6_RH.shape.gii -reduce SUM -roi ${DATA_DIR}/sub-${sub}_LABEL${network}_RH.shape.gii`


	#Save SA as .csv
	echo "sub-${sub}, NETWORK${network}, ${LH_SA}, ${RH_SA}" >> ${CODE_DIR}/NETWORK_SA_SUB_NET_LH_RH_230214.csv
done
done
