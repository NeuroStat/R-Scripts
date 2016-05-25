####################
#### TITLE:     Two functions to copy headers from existing nifti file to target.
#### Contents:
####
#### Source Files: https://github.com/NeuroStat/R-scripts
#### First Modified: 25/05/2016
#### Author: Han Bossier
#### Notes:
#################

# Two functions, in case you want to copy a header from 
# an existing nifti file (which you have read in using readNIfTI() ) to a custom 3D/4D array. 


##
###############
### Package
###############
##

require(oro.nifti)


##
###############
### Copy all fields
###############
##

# Make sure you understand the specific fields you are copying. Copying the wrong files can give weird results!

# Copy header from source to target nifti image
    # Input:  target is a 3D/4D array
              # source is a nifti file
              # pixdim is vector of length 8 with TR, voxel dimensions, and ? ? ? ?
CopyHeader <- function(target, source, pixdim){
	EndNIFTI <- nifti(img = target,
				sizeof_hdr = source@sizeof_hdr,
				datatype = source@datatype,
				db_name = source@db_name,
				extents = source@extents,
				regular = source@regular,
				dim_info = source@dim_info,
				dim_ = source@dim_,
				intent_p1 = source@intent_p1,
				intent_p2 = source@intent_p2,
				intent_p3 = source@intent_p3,
				intent_code = source@intent_code,
				bitpix = source@bitpix,
				slice_start = source@slice_start,
				pixdim = source@pixdim,
				vox_offset = source@vox_offset,
				scl_slope = source@scl_slope,
				scl_inter = source@scl_inter,
				slice_end = source@slice_end,
				slice_code = source@slice_code,
				xyzt_units = source@xyzt_units,
				slice_duration = source@slice_duration,
				toffset = source@toffset,
				glmax = source@glmax,
				glmin = source@glmin,
				descrip = source@descrip,
				aux_file = source@aux_file,
				sform_code = source@sform_code,
				quatern_b = source@quatern_b,
				quatern_c = source@quatern_c,
				quatern_d = source@quatern_d,
				qoffset_x = source@qoffset_x,
				qoffset_y = source@qoffset_y,
				qoffset_z = source@qoffset_z,
				srow_x = source@srow_x,
				srow_y = source@srow_y,
				srow_z = source@srow_z,
				intent_name = source@intent_name,
				magic = source@magic,
				extender = source@extender,
				reoriented = source@reoriented
				)
			# For some reason, the qform_code cannot be written directly. We add it here
			EndNIFTI@qform_code <- source@qform_code
			EndNIFTI@pixdim <- pixdim
	return(EndNIFTI)
}


##
###############
### Copy pixdim and qform code
###############
##

# In order to have a proper transformation using flirt, one needs to have a qform_code.
# When specifying a qform_code, you will also need a specific pixdim

# This next function only copies those two fields. The pixdim default value is the one used in the IMAGEN data base (I presume). 

# Function to write nifti header that will be needed for correct transformation
WRITENIFTI <- function(target, source, dim, pixdim = c(-1,3,3,3,1,1,1,1)){
	EndNIFTI <- nifti(img = target, dim = dim,
								datatype = 16)
		EndNIFTI@qform_code <- source@qform_code
		EndNIFTI@pixdim <- pixdim
	return(EndNIFTI)
}
