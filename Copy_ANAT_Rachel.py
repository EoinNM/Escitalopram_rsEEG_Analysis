import os
from os import *
import dicom as dcm
import shutil
import glob
from utils.utils import *
from variables.variables import *

population = ["Subject_1", "Subject_2", "Subject_3"]
days       = ["Day_1", "Day_2", "Day_3"]
workspace = '' #path to where you want to copy the data

def mkdir_path(path):
    import os
    import errno
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else: raise
    return path

def pull_T1(population, zfs, workspace, days):

    for subject in population:
        for day in days:
            
            #enter the path to the folder containing the DICOM data as a string. 
            # This depends on the structure of the files. I assume it goes path, 
            #then a list of subjects, and then a list of each day, within each are the files?
            src_dcm_dir = os.path.join("", subject, day) 
            #make a directory in the workspace that contaiuns a folder for each subject in the population i.e. the subject directory
            sub_dir    = mkdir_path(os.path.join(workspace, 'MCP_ANAT_DATA', 'NII', subject)) 
            #within the subject dir, create folder for where the dicoms will go
            dcm_dir = mkdir_path(os.path.join(workspace, 'MCP_ANAT_DATA', 'DCM', subject, day))
            # This creates a separate directory for each of the 4 anatomical files - rename as you like
            anat_1_dir = mkdir_path(os.path.join(sub_dir, 'ANATOMICAL_1', day, 'NII'))
            anat_2_dir = mkdir_path(os.path.join(sub_dir, 'ANATOMICAL_2', day, 'NII'))
            anat_3_dir = mkdir_path(os.path.join(sub_dir, 'ANATOMICAL_3', day, 'NII'))
            anat_4_dir = mkdir_path(os.path.join(sub_dir, 'ANATOMICAL_4', day, 'NII'))

            # read all DICOMs in the source directory
            all_dicoms_1 = [os.path.join(src_dcm_dir, img) for img in  os.listdir(src_dcm_dir_)]
            # read MPRAGE images
            mprage_dicoms = []
            for img in sorted(all_dicoms):
                series_description = dcm.read_file(img).SeriesDescription
                if "MPRAGE" in series_description: #where it says "MPRAGE", you will have to change this to the descriptor in your own data headers
                    mprage_dicoms.append(img)

            # copy MPRAGE dicoms to workspace - this should populate the DICOM directory with files for each subject and day
            for file in mprage_dicoms:
                shutil.copy(file, dcm_dir)

            # convert DICOMS to NIFTI
            os.system('dcm2niix -o %s %s' % (nii_dir, dcm_dir))

pull_T1(population, zfs, workspace, days)