
#Ajani Blackwood

##Wrote script to copy specific files metting specific criterion
##from the RAVDESS dataset. This was needed because even though
##the audio and videodata had separate folders per actor,they did 
##not do this for the facial landmark tracking

##This script was written to easily grab any combination of files from the
##RAVDESS dataset. It is huge with over 7000 files and the windows search 
##function does not do such a good job

##If I want a certain combination of emotions, genders, statements, intensities
##This script allows me to grab them from the gigantic dataset

import os
from tkinter import filedialog, Tk
from tkinter import messagebox
import shutil

root = Tk()
root.withdraw()

identified = {} #Shows the identified .csv files within the directory

##This will make more sense after reading this: https://zenodo.org/record/1188976#.YJc_8MCSmMo

###Example of a combination of files to extract.

## 01-01-03-02-01-01

##Look for all files with....

# 01-tracked AV (All of them start with this)
# 01-speech trials
# 03-happy speech trials
# 02-strong emotional intensity
# 01- first statement
# 01- first male actor (since odd numbers for the guys)

##So the more criteria the moe narrowed doen
##Above is just an example of what combination you can use, but you can try others




def findname(folder,filestart):
    ##This finds what the file starts with

    taken = {} #This stores the names of the files to grab if the start with "filestart"
    for subdir, dirs, files in os.walk(folder):#Iterate through the folders to search for files meeting criterion
        for filename in files:
            file = subdir + os.path.join("/", filename) #Add the needed ending to the file directory
            identified[filename] = file#Identifies all files in the folder and lists them
            
            if filestart[0:len(filestart)] in filename:
                taken[filename] = file #Add files starting with the relevant criterion to the "taken dictionary"

    return taken

##The next portion asks you to open the folder with the RAVDESS files and what the relevant ones start with
messagebox.showinfo('Folder','Please open target folder.')
target_folder = filedialog.askdirectory()

startwith = input("What does the file start with?: ")

identified = findname(target_folder,startwith)

print (identified)##This line is not needed but it is good to see if t is identifying the files you expect it to

#Choose a separate folder to put the extracted files in and write them there
messagebox.showinfo("Destination", "Open the folder to copy the files to")
copyto = filedialog.askdirectory()

for key, file in identified.items():
    shutil.copy(file, copyto)


