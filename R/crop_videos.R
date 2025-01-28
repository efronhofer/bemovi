#' Function to CROP VIDEOS THAT NEED TO BE CROPPED (by Fanny Gonguet)
#' 
#' Function to reduce the time of analyses for too heavy documents by cropping particles coordinates. Only them whithin the cropped frame will continue the analyse.
#' @param to.data path to the working directory
#' @param particle.data.folder directory in which text file data describing particules morphology and coordiantes are stored
#' @param max.weight variable to set as weight from wich text file data should be cropped (Mb)
#' @param coordinates.threshold variable to set as the minimum coordinates from wich to crop the particles coordinates (pixels)
#' @param width width in pixels of the original videos
#' @param height height in pixels of the original video
#' @return returns nothing (NULL)
#' @export

crop_videos <- function(to.data, particle.data.folder, max.weight, coordinates.threshold, width, height) {
  #converting MB to bytes
  max.weight <- max.weight / 0.000001
  # go to particale.data.folder
  setwd(particle.data.folder)
  #creating a dataframe with files name and files sizes whithin the directory
  sizes <-system("echo  \"File_Name\tSize\" && ls -l | awk '{print $9, $5}' ",intern=TRUE)
  file_summary <- read.table(text = sizes,header=TRUE)
  #keeping only .txt files
  #library(dplyr)
  particle_file_summary <- subset(file_summary, grepl(".txt", File_Name))

  ##############################################################################
  #creating a function that subsets files for which filesize > max.weight
  #It copies the original dataset in a new folder (new.dir)
 
  #copy_filter_function <- function(filesize, filename, max.weight){
  #filesize argument referring to sizes of files
  #filename argument referring to names of files
  #max.weight defining the maximal weight upon which particles coordinates are cropped

  
  copy_filter_function <- function(filesize,filename,max.weight){
    #loop applying if filesize > max.weight
    if (filesize > max.weight) {
      #saying user that this file size is > max.weight
      print(paste(filename, "size >  max.weight"))
      
      #creating a dataframe with only the .txt files with a filesize > wax.weight
      particle_file_summary_filtered <<- particle_file_summary[particle_file_summary$Size > max.weight, ]
    } else{
      #saying user when filesize < max.weight
      print(paste(filename, "size <  max.weight"))}}
  ##############################################################################
  
  #creating a vector for results (length of the vector is length of dataframe 
  #with only the .txt files with a filesize > wax.weight)
  results <- numeric(nrow(particle_file_summary))
  #Loop to apply the copy_filter_function to each pair of elements from both vectors
  for (i in 1:nrow(particle_file_summary)) {
    results[i] <- copy_filter_function(particle_file_summary$Size[i], particle_file_summary$File_Name[i],max.weight)  # Apply the function using the values from each row
  }
  
  ##############################################################################
  #create a function to crop analysed particules within a certain frame
  
  #width and height and the resolution of each frames used to produce videos and thus text files
  #coordinates.threshold variable to set as the minimum coordinates from wich to crop the particles coordinates (pixels)
  crop_function <- function (dataset, coordinates.threshold, width, height) {
    #defining the crop coordinates (according to coordinates.threshold)
    Xmin <- coordinates.threshold
    Xmax <- width - coordinates.threshold
    Ymin <- coordinates.threshold
    Ymax <- height - coordinates.threshold
    #initializing an empty dataframe to store the "cropped dataset"
    subset_dataset <- data.frame()
    #cropping the dataset an put it into the new dataset "subset_dataset"
    subset_dataset <- dataset[(dataset$X >= Xmin & dataset$X <= Xmax) & (dataset$Y >= Ymin & dataset$Y <= Ymax), ]
  }
  ##############################################################################
  #applying the crop_function to each file of the "particle_file_summary"
  
  #vector with files names from "particle_file_summary"
  txt_files <- particle_file_summary_filtered$File_Name
  
  #initializing a list to store the data from all .txt files
  all_data <- list()
  #Loop through each file and read its content
  for (file in txt_files) {
    # Read the current .txt file
    data <- read.table(file, header = TRUE)
    # Add the data to the list
    all_data[[file]] <- data
  }
  
  #initializing a list to store results
  results <- vector("list",length(all_data))
  #For loop through each file read in "all_data"
  #all_data[[i]] refers to the ith file
  for (i in 1:length(all_data)) {
    # Reading the current file into a data frame
    current_dataset <- all_data[[i]]
    # Applying the crop_function on the current_dataset
    cropped_dataset <- crop_function(current_dataset, coordinates.threshold, width, height)
    #storing it in the "result" vector
    results[[i]] <- cropped_dataset
    #saving the cropped_dataset
    save_dir <- paste0(to.data, particle.data.folder, "/", basename(names(all_data)[i]))
    write.table(cropped_dataset, file = save_dir, sep = "\t", row.names = FALSE, quote = FALSE)
    
    #Checking if the file was saved successfully
    if (file.exists(save_dir)) {
      print(paste(save_dir, "saved successfully"))
      return(NULL)
    } else {
      warning(paste("Failed saving", save_dir))
      return(FALSE)
    }
  }
  setwd(to.data)
  return(NULL)
}
