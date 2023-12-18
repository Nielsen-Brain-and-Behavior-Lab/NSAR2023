%PURPOSE: Calculate language lateralization in HCP subjects using
%Fedorenko lab methods
%INPUTS: t-statistic maps from story-math contrast (preprocessed and
%fsaverage6 resolution), LanA atlas in fsaverage6 resolution
%OUTPUTS: .csv file contatining lateralization values per participant
%DEPENDENCIES: gifti
%package(https://www.gllmflndn.com/software/matlab/gifti/)

%Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License
%2023

%% PATHS
atlas_path = '/fslgroup/fslg_spec_networks/compute/code/HCP_analysis/LanA_atlas/FS';
tstat = '/fslgroup/grp_proc/compute/HCP_analysis/HCP_download';
outdir = '/fslgroup/fslg_spec_networks/compute/code/HCP_analysis/lang_lat/lana_mask';

%path to gifti v2
addpath('/fslgroup/fslg_spec_networks/compute/research_bin/gifti-main')
%% SUBJID LIST

%Read in the subjids text file
    filename = '/nobackup/scratch/grp/fslg_spec_networks/code/HCP_analysis/lang_lat/lana_mask/lang_subs.txt';
    delimiter = {''};
    formatSpec = '%s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string',  'ReturnOnError', false);
    fclose(fileID);
    sublist = [dataArray{1:end-1}];
    clearvars filename delimiter formatSpec fileID dataArray ans;


%% LANA ATLAS

%use GIFTI package (CBIG repo) to load GIFTI files
    lh_file = gifti(fullfile(atlas_path,'LH_LanA_n804.shape.gii'));
	rh_file = gifti(fullfile(atlas_path,'RH_LanA_n804.shape.gii'));
    
%binarize LH and RH data
    %set threshold as top 10%
    concatenated_vector = [lh_file.cdata; rh_file.cdata];
    nonzero_elements = concatenated_vector(concatenated_vector ~= 0); %find nonzero elements
    threshold = prctile(nonzero_elements, 90);
    
    %binarize
    %LH
    lh_labels = zeros(size(lh_file.cdata)); % Initialize with zeros
    lh_labels(lh_file.cdata > threshold) = 1;      
    %RH
    rh_labels =zeros(size(rh_file.cdata));
    rh_labels(rh_file.cdata > threshold) = 1;

%% LOOP

%Initialize empty vectors
lh_count_list = []; 
rh_count_list = []; 



for subid = 1:length(sublist)
    sub=sublist(subid);

    %load tstat gifti files
    lh_lang = gifti(char(fullfile(tstat,sub,strcat('sub-', sub, '_tstat1_L_fsaverage6.func.gii'))));
	rh_lang = gifti(char(fullfile(tstat,sub,strcat('sub-', sub, '_tstat1_R_fsaverage6.func.gii'))));    
    
    % Create logical masks for left and right hemisphere based on the atlas
    lh_mask = (lh_labels == 0); % Create a logical mask for the left hemisphere
    rh_mask = (rh_labels == 0); % Create a logical mask for the right hemisphere

    % Apply the masks to set corresponding values to 0 in the language task data
    lh_lang.cdata(lh_mask) = 0;
    rh_lang.cdata(rh_mask) = 0;
    
    %Threshold tstat to top 10% of vertices
        concatenated_vector = [lh_lang.cdata; rh_lang.cdata];
        nonzero_elements = concatenated_vector(concatenated_vector ~= 0); %find nonzero elements
        threshold = prctile(nonzero_elements, 90);

        %LH
        lh_thresh = zeros(size(lh_lang.cdata)); % Initialize with zeros
        lh_thresh(lh_lang.cdata > threshold) = 1;     
        %RH
        rh_thresh = zeros(size(rh_lang.cdata));
        rh_thresh(rh_lang.cdata > threshold) = 1;
        
    %Count the number of vertices that survive threshold in LH & RH
    lh_count = sum(lh_thresh);
    rh_count = sum(rh_thresh);
    
    %Append counts to list vectors
    lh_count_list = [lh_count_list, lh_count];
    rh_count_list = [rh_count_list, rh_count];   
    
end


% Create a table with subject_list, lh_count, and rh_count as variables
dataTable = table(sublist, lh_count_list', rh_count_list', 'VariableNames', {'SUBJID', 'LH_VERT', 'RH_VERT'});
    
% Define the filename for the output CSV file
output_filename = fullfile(outdir,'HCP_langlat_lanamask_230929.csv');

% Use the writetable function to save the table to a CSV file
writetable(dataTable, output_filename);
