#' Function to calculate densities and summarize morphology and movement at the population (sample) level
#' 
#' Takes the data comprising the information for each frame and calculates summary statistics such as mean and sd (for all morphology metrics) and mean, sd and min/max 
#' for some of the movement metrics along the trajectory. Values are rounded to the second decimal. 
#' 
#' @param traj.data dataframe with the information on morphology and movement for each frame (either "trajectoty.data" or "trajectory.data.filtered")
#' @param sum.data data.table with the aggregated morphology and movement information for each trajectory ("morph_mvt")
#' @param write logical argument to indicate whether aggregated information should be saved to disk (file name: "Population_Data.RData")
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param video.description.folder directory with the video description file
#' @param video.description.file name of the video description file
#' @param total_frames length of video analysed in frames
#' @return returns a data.table with the population densities, biovolume as well as the aggregated morphology and movement information for each population (sample)
#' @export
#' @examples
#' summarize_populations()

summarize_populations <- function(traj.data, sum.data, write=FALSE, to.data, merged.data.folder, video.description.folder, video.description.file, total_frames, coordinates.threshold, width, height){
  
  # checks whether frames per second are specified
  if(!exists("fps") ) stop("frames per second not specified (fps)")
  # checks whether the sample volume is specified
  if(!exists("measured_volume") ) stop("measured volume not specified (measured_volume)")
  
  # results object from video description file
  pop_output <- read.table(paste(to.data, video.description.folder, video.description.file, sep = ""), sep = "\t", header = TRUE)
  cropped_files <- read.table(paste(to.data, particle.data.folder, "cropped_files.txt",sep= ""),sep = "\t", header = TRUE)
  library(dplyr)
  pop_output <- pop_output  %>%
    left_join(cropped_files, by = "file")
  
  # now add the population densities
  pop_count_table <- tapply(sum.data$N_frames, sum.data$file, sum)/total_frames
  
  help_reorder <- match(pop_output$file, names(tapply(sum.data$N_frames, sum.data$file, sum)/total_frames))
  
  pop_output$indiv_per_frame <- 0
  
  ##############################################################################
  #if the frame has been cropped, densities calculated are within the cropped frame area
  #here we infer densities of the global frame area from the one calculated from
  #the cropped one
  
  # calculating area of the global frame
  total_frame_area <- width*height
  
  # calculating area of the cropped frame
  # removing "coordinates threshold" from both sides of the global frame
  cropped_frame_area <- (width - (2*coordinates.threshold)) * (height - (2*coordinates.threshold))
  
  #calcul of densities when frame has been cropped
  pop_count_table_cropped <- pop_count_table * (total_frame_area/cropped_frame_area)
  
  #different results if or if not cropped frames
  pop_output$indiv_per_frame <- ifelse(
    pop_output$Cropped == "yes",
    pop_count_table_cropped[help_reorder], #if the frame has been cropped, new calculus
    pop_count_table[help_reorder]#otherwise, densities not recalculated
  )
  
  pop_output$indiv_per_volume <- 0
  pop_output$indiv_per_volume <- pop_output$indiv_per_frame/measured_volume
  
  ##############################################################################
  #if the frame has been cropped, bioareas calculated are within the cropped frame area
  #here we infer bioareas of the global frame area from the one calculated from
  #the cropped one
  #same method as before for densities
  # add the mean of total area per frame (bioarea by frame)
  pop_count_table2 <- tapply(traj.data$Area,list(as.factor(traj.data$file),as.factor(traj.data$frame)),sum)
  help_reorder <- match(pop_output$file, names(apply(pop_count_table2,1,sum,na.rm=T)))
  pop_output$bioarea_per_frame <- 0
  pop_output$bioarea_per_frame <- ifelse(
    pop_output$Cropped == "yes",
    as.numeric(apply(pop_count_table2,1,sum,na.rm=T)/total_frames)* (total_frame_area/cropped_frame_area),
    as.numeric(apply(pop_count_table2,1,sum,na.rm=T)[help_reorder])/total_frames
  )
  
  # add the mean of total area per volume (bioarea by volume; by Isabelle Gounand)
  pop_output$bioarea_per_volume <- 0
  pop_output$bioarea_per_volume <- pop_output$bioarea_per_frame/measured_volume
  
  # first get file from id
  help_reorder <- match(pop_output$file, names(tapply(sum.data$mean_major,sum.data$file,mean,na.rm=T)))
  
  # get morphology
  pop_output$major_mean <- NA
  pop_output$major_sd <- NA
  pop_output$minor_mean <- NA
  pop_output$minor_sd <- NA
  
  pop_output$major_mean <- as.numeric(tapply(sum.data$mean_major,sum.data$file,mean,na.rm=T)[help_reorder])
  pop_output$major_sd <- as.numeric(tapply(sum.data$mean_major,sum.data$file,sd,na.rm=T)[help_reorder])
  pop_output$minor_mean <- as.numeric(tapply(sum.data$mean_minor,sum.data$file,mean,na.rm=T)[help_reorder])
  pop_output$minor_sd <- as.numeric(tapply(sum.data$mean_minor,sum.data$file,sd,na.rm=T)[help_reorder])
  
  # calculate gross speed
  sum.data$gross_speed <- sum.data$gross_disp/sum.data$duration 
  
  # get movement
  pop_output$gross_speed_mean <- NA
  pop_output$gross_speed_sd <- NA
  pop_output$net_speed_mean <- NA
  pop_output$net_speed_sd <- NA
  pop_output$sd_turning_mean <- NA
  
  # mean gross speed is not returned yet --> calc here
  sum.data$gross_speed <- sum.data$gross_disp/sum.data$duration
  
  pop_output$gross_speed_mean <- as.numeric(tapply(sum.data$gross_speed,sum.data$file,mean,na.rm=T)[help_reorder])
  pop_output$gross_speed_sd <- as.numeric(tapply(sum.data$gross_speed,sum.data$file,sd,na.rm=T)[help_reorder])
  
  pop_output$net_speed_mean <- as.numeric(tapply(sum.data$net_speed,sum.data$file,mean,na.rm=T)[help_reorder])
  pop_output$net_speed_sd <- as.numeric(tapply(sum.data$net_speed,sum.data$file,sd,na.rm=T)[help_reorder])
  
  pop_output$sd_turning_mean <- as.numeric(tapply(sum.data$sd_turning,sum.data$file,mean,na.rm=T)[help_reorder])
  
  #output population summary data
  if (write==TRUE){save(pop_output, file = paste0(to.data, merged.data.folder,"Population_Data.RData"))}
  return(as.data.frame(pop_output))
}
