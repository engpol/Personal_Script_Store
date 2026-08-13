#@ File (label="Select input image:", style="file") imagePath
#@ String (label="Select channel to crop:", choices={"Left", "Right"}, style="radioButton") channelChoice

// 1. Open the selected image
open(imagePath);

// Get the original title to rename the cropped version later
originalTitle = getTitle();

// 2. Get the dimensions of the opened image
width = getWidth();
height = getHeight();

// Calculate the width of a single channel (assuming a perfect 50/50 horizontal split)
halfWidth = width / 2;

// 3. Create the ROI based on the user's choice
if (channelChoice == "Left") {
    // x=0, y=0, w=halfWidth, h=height
    makeRectangle(0, 0, halfWidth, height);
} else if (channelChoice == "Right") {
    // x=halfWidth, y=0, w=halfWidth, h=height
    makeRectangle(halfWidth, 0, halfWidth, height);
}

// 4. Crop the image to the defined ROI
run("Crop");

// 5. Rename the image so you know which channel it is
rename(originalTitle + " - " + channelChoice + " Channel");

// Print a confirmation to the log window
print("Successfully cropped the " + channelChoice + " channel for: " + originalTitle);