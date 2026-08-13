#@ ImagePlus (label="Reference Image (Keep these dimensions):") refImg
#@ ImagePlus (label="Image to Resize (Will be scaled):") scaleImg
#@ String (label="Interpolation Method:", choices={"Bilinear", "Bicubic", "None"}, style="radioButton") interp

// 1. Select the reference image using its ID and get its dimensions
selectImage(refImg);
targetW = getWidth();
targetH = getHeight();

// 2. Select the image that needs to be resized
selectImage(scaleImg);
originalTitle = getTitle();

// 3. Run the Scale command
// 'average' helps preserve signal when downsampling
// 'create' ensures a new image is generated rather than overwriting the original
run("Scale...", "x=- y=- width=" + targetW + " height=" + targetH + " interpolation=" + interp + " average create");

// 4. Rename the newly created scaled image for clarity
rename(originalTitle + " - Scaled");

// 5. Print a confirmation log
print("Successfully scaled '" + originalTitle + "' to " + targetW + " x " + targetH + " pixels.");
print("You can now merge this with your Reference Image.");