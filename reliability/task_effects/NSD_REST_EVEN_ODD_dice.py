#!/usr/bin/env python
# Purpose: Create a .csv file of Nx17 dice coefficients to assess similarity between MSHBM parcs
# Input: ind_parcellation output from Kong2019 pipeline
# Output: 1 Nx17 .csv file with dice coefficients
#
# Note: Users may need to run 'which python' and paste the correct path following the shebang on line one.
#
# Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License (2021)

from pathlib import Path
import time
import os
from os.path import dirname, join as pjoin
import sys
import scipy.io #loads .mat files
import csv
import numpy as np
import pandas as pd

# create function to get index positions
def get_index_positions(list_of_elems, element):
	index_pos_list = []
	index_pos = 0
	while True:
		try:
			index_pos = list_of_elems.index(element, index_pos)
			index_pos_list.append(index_pos)
			index_pos += 1
		except ValueError as e:
			break
	return index_pos_list

# Create 2D matrices of the dice coefficient for each network
count=0
sub_count=0
directory = '/fslgroup/fslg_fun_conn/compute/NSD_analysis/CBIG2016_preproc_FS6_REST/'
for sub in os.scandir(directory): #loop through each subject in the preproc dir
	sub_count=(sub_count+1)
	for network in range(0,18): #Range must be set to actual number of networks +1
		count=(count+1)

		# path to test_set subjects
		test_dir: str="/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_EVEN_REST/generate_individual_parcellations/ind_parcellation/test_set"
		test_name = "Ind_parcellation_MSHBM_sub" + str(sub_count) + "_w200_MRF30_matched.mat"
		test_sub = pjoin(test_dir, test_name)
		test_file = scipy.io.loadmat(test_sub) #Load first sub .mat file
		test_rh = np.squeeze(test_file['rh_labels']) 
		test_rh_list = test_rh.tolist()			
		test_lh = np.squeeze(test_file['lh_labels'])
		test_lh_list = test_lh.tolist()
		# combine Rh and Lh labels
		test_comb2 = np.append(test_rh_list,test_lh_list) #Combine RH and LH labels files
		test_comb = test_comb2.tolist()
	
		# path to second subject
		sec_dir: str="/fslgroup/fslg_fun_conn/compute/NSD_analysis/parc_output_fs6_NSD_ODD_REST/generate_individual_parcellations/ind_parcellation/test_set"
		sec_name = "Ind_parcellation_MSHBM_sub" + str(sub_count) + "_w200_MRF30_matched.mat"
		sec_sub = pjoin(sec_dir, sec_name)
		sec_file = scipy.io.loadmat(sec_sub)
		sec_rh = np.squeeze(sec_file['rh_labels'])
		sec_rh_list = sec_rh.tolist()
		sec_lh = np.squeeze(sec_file['lh_labels'])
		sec_lh_list = sec_lh.tolist()
		# Combine Rh and Lh labels
		sec_comb2 = np.append(sec_rh_list,sec_lh_list)
		sec_comb = sec_comb2.tolist()

		#find total and common vertices between subjects
		test_tot = test_comb.count(network) #Total vertices for $network in test subject (sub1)
		sec_tot = sec_comb.count(network) #Total vertices for $network for second subject (sub2)

		index_pos_list = get_index_positions(test_comb, network) #Get positions/order for $network for sub1
		test_pos_list = index_pos_list 
		index_pos_list = None

		index_pos_list = get_index_positions(sec_comb, network) #Get positions/order for $network for sub2
		sec_pos_list = index_pos_list 
		index_pos_list = None

		common = len([x for x in test_pos_list if x in sec_pos_list]) #Compare positions for $network for both subjects
		denominator = sec_tot + test_tot #Combine total number of vertices with $network
		dice = (2 * common) / denominator if denominator != 0 else 0  # calculate dice coefficient

		if count==1:
			runningList = [[str(sub.name), network, dice]] 
			df = pd.DataFrame(runningList, columns=['SUBJID', 'Network', 'Dice'])
		else:
			runningList = [[str(sub.name), network, dice]]
			df = df.append(pd.DataFrame(runningList, columns=['SUBJID', 'Network', 'Dice']), ignore_index=True)

#Save the dataframe as .csv
r_name = "NSD_REST_EVEN_ODD_dice_LONG_230302.csv" #file name
df.to_csv(r_name, index=False, index_label=None) #save dataframe to csv
