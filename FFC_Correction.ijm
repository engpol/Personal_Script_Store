//- - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - -- - - -- -- - - - - - -- -- - - - - - -- -- - -
// PARAMETERS
//- - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - -- - - -- -- - - - - - -- -- - - - - - -- -- - -

#@ File(label="Folder Containing Images to apply FFC", value = "C:/", style="directory") sample_folder
#@ File(label="Flat-field Image", value = "C:/", style="file") ffimage

run("Fresh Start"); //ALWAYS INCLUDE
setBatchMode(false);
setOption("ExpandableArrays",true);
//- - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - -- - - -- -- - - - - - -- -- - - - - - -- -- - -
// FUNCTIONS
//- - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - -- - - -- -- - - - - - -- -- - - - - - -- -- - -

function ImageFilesOnlyArray (arr) {
	//pass array from getFileList through this e.g. NEWARRAY = ImageFilesOnlyArray(NEWARRAY);
	f=0;
	files = newArray;
	print(stacks_filelist[f])
	for (i = 0; i < arr.length; i++) {
		if(endsWith(arr[i], ".tif") || endsWith(arr[i], ".tiff") || endsWith(arr[i], ".TIF") || endsWith(arr[i], ".TIFF") || endsWith(arr[i], ".nd2") || endsWith(arr[i], ".LSM") || endsWith(arr[i], ".czi") || endsWith(arr[i], ".jpg") ) {   //if it's a tiff image add it to the new array
			files[f] = arr[i];
			f = f+1;
		}
	}
	arr = files;
	arr = Array.sort(arr);
	return arr;
	
}

//- - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - -- - - -- -- - - - - - -- -- - - - - - -- -- - -
// VARIABLES
//- - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - -- - - -- -- - - - - - -- -- - - - - - -- -- - -

stacks_filelist = getFileList(sample_folder);
Array.print(stacks_filelist);
stacks_images = ImageFilesOnlyArray(stacks_filelist);
stacks_length = lengthOf(stacks_images);


//- - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - -- - - -- -- - - - - - -- -- - - - - - -- -- - -
// CODE
//- - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - - - - - -- -- - -- - - -- -- - - - - - -- -- - - - - - -- -- - -

File.mkdir(sample_folder + File.separator + "Corrected_Images");

for (i = 0; i < stacks_length; i++) {
	open(ffimage);
	rename("FLAT_FIELD");
	open(stacks_images[i]);
	run("BaSiC ", "processing_stack="+stacks_images[i]+" flat-field=FLAT_FIELD dark-field=None shading_estimation=[Skip estimation and use predefined shading profiles] shading_model=[Estimate flat-field only (ignore dark-field)] setting_regularisationparametes=Automatic temporal_drift=Ignore correction_options=[Compute shading and correct images] lambda_flat=0.50 lambda_dark=0.50");
	selectImage("Corrected:"+stacks_images[i]);
	save(sample_folder + File.separator + "Corrected_Images" + File.separator + stacks_images[i]);
	close("*");
}

