% Purpose: Split surface-project preprocessed runs into thirds.
% Inputs: Fsaverage6-projected CBIG2016 preproc output.
% Outputs: 3 nifti files per run (5-min increments for HCP data).
%
% Written by M. Peterson, Nielsen Brain and Behavior Lab, under MIT License 2023

% To run: 
%	 0. Source your CBIG config script with paths to softwares.
%	 1. Claim computing resources using salloc (ex: `salloc --mem-per-cpu 300G --time 48:00:00 --x11`)
%	 2. Load matlab module: `ml matlab/r2018b`
%	 3. Enter the command `LD_PRELOAD= matlab`


% Set paths.
preproc_out = '/fslgroup/fslg_autism_networks/compute/HCP_analysis/CBIG2016_preproc_FS6_ALL'; 
seslist = [ "2" "4" ];

%Load text file
    filename = '/nobackup/scratch/grp/fslg_spec_networks/code/HCP_analysis/CBIG2016_preproc/subjids/4SESS_IDS.txt';
    delimiter = {''};
    % Format for each line of text:
    %   column1: text (%s)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%s%[^\n\r]';
    % Open the text file.
    fileID = fopen(filename,'r');
    % Read columns of data according to the format.
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string',  'ReturnOnError', false);
    % Close the text file.
    fclose(fileID);
    sublist = [dataArray{1:end-1}];
    clearvars filename delimiter formatSpec fileID dataArray ans;


for subid = 1:length(sublist)
	subj=sublist(subid); 
	for ses = seslist
		%Var names
		subj=sublist(subid); 
		RH_NAME=strcat('rh.', subj, '_bld00', ses, '_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz'); 
		RH_FULL=fullfile(preproc_out,subj,subj,'surf',RH_NAME);		
		LH_NAME=strcat('lh.', subj, '_bld00', ses, '_rest_skip4_mc_resid_bp_0.009_0.08_fs6_sm6_fs6.nii.gz');
		LH_FULL=fullfile(preproc_out,subj,subj,'surf',LH_NAME);
		output_path=strcat(preproc_out,'/',subj,'/',subj,'/surf');

		% Load the NIFTI files using mri_read
		data_rh = MRIread(char(RH_FULL));
		data_lh = MRIread(char(LH_FULL));


		% Calculate the size of each third of the timeseries
        n = size(data_rh.vol, 4); %size of the fourth dimension = # of vols
		third = floor(n_timepoints / 3); %divide into thirds (whole integers)
        rh_parts = mat2cell(data_rh.vol, size(data_rh.vol, 1), size(data_rh.vol, 2), size(data_rh.vol, 3), [third, third, n-2*third]);
        lh_parts = mat2cell(data_lh.vol, size(data_lh.vol, 1), size(data_lh.vol, 2), size(data_lh.vol, 3), [third, third, n-2*third]);

        % Assign each part to a separate variable
        rh_part1 = data_rh; 
        rh_part2 = data_rh;
        rh_part3 = data_rh;
        lh_part1 = data_lh;
        lh_part2 = data_lh;
        lh_part3 = data_lh;
        rh_part1.vol = 0;
        rh_part2.vol = 0;
        rh_part3.vol = 0; 
        lh_part1.vol = 0; 
        lh_part2.vol = 0;
        lh_part3.vol = 0;
        rh_part1.nframes = size(rh_parts{1}, 4);
        rh_part2.nfrmaes = size(rh_parts{2}, 4);
        rh_part3.nframes = size(rh_parts{3}, 4);
        lh_part1.nframes = size(lh_parts{1}, 4);
        lh_part2.nframes = size(lh_parts{2}, 4);
        lh_part3.nframes = size(lh_parts{3}, 4);
        rh_part1.vol = rh_parts{1}; %fill with a third of the data
        rh_part2.vol = rh_parts{2};
        rh_part3.vol = rh_parts{3};
        lh_part1.vol = lh_parts{1};
        lh_part2.vol = lh_parts{2}; 
        lh_part3.vol = lh_parts{3};         
        
		% Write the third of the timeseries to NIFTI files 
        MRIwrite(rh_part1, char(strcat(output_path, '/rh_', subj, '_bld00', ses, '_third_1.nii.gz')));
		MRIwrite(rh_part2, char(strcat(output_path, '/rh_', subj, '_bld00', ses, '_third_2.nii.gz')));
		MRIwrite(rh_part3, char(strcat(output_path, '/rh_', subj, '_bld00', ses, '_third_3.nii.gz')));
		MRIwrite(lh_part1, char(strcat(output_path, '/lh_', subj, '_bld00', ses, '_third_1.nii.gz')));
		MRIwrite(lh_part2, char(strcat(output_path, '/lh_', subj, '_bld00', ses, '_third_2.nii.gz')));
		MRIwrite(lh_part3, char(strcat(output_path, '/lh_', subj, '_bld00', ses, '_third_3.nii.gz'))); 

end
end
