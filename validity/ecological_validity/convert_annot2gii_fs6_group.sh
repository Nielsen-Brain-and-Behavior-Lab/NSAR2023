#!/bin/bash

#Purpose: Create label files in fs6 space - from GROUP parcellation files
#Inputs: parc2annot annotation files
#Outputs: .label.gii parcellation files
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2022

#SET PATHS- ODD RUNS
HOME=/fslgroup/grp_hcp/compute
OUTDIR=${HOME}/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/MSHBM_GROUP_vis5000

#Create .label.gii parcellation files
for SUB in GROUP; do
	#Resample LH annot
	mri_surf2surf --srcsubject fsaverage --sval-annot ${OUTDIR}/sub-${SUB}_lh.annot --trgsubject fsaverage6 --hemi lh --trgsurfval ${OUTDIR}/sub-${SUB}_fs6_lh --trg_type annot
	#Resample RH annot
	mri_surf2surf --srcsubject fsaverage --sval-annot ${OUTDIR}/sub-${SUB}_rh.annot --trgsubject fsaverage6 --hemi rh --trgsurfval ${OUTDIR}/sub-${SUB}_fs6_rh --trg_type annot

	#LH label file
	mris_convert --annot ${OUTDIR}/sub-${SUB}_fs6_lh.annot ${FREESURFER_HOME}/subjects/fsaverage6/surf/lh.white ${OUTDIR}/sub-${SUB}_lh.label.gii
	#RH label file
	mris_convert --annot ${OUTDIR}/sub-${SUB}_fs6_rh.annot ${FREESURFER_HOME}/subjects/fsaverage6/surf/rh.white ${OUTDIR}/sub-${SUB}_rh.label.gii
done

