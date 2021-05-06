#' Function to make overlays using the subtitle files,
#' 
#' Function creates subtitle comands for every cell in every frame of the videos, using the x/y coordinates.
#' Then ffmpeg is called to burn the overlay subtitles on, and save a compressed video.
#' @param to.data path to the working directory
#' @param traj.data filtered trajectory file
#' @param raw.avi.folder path to the folder containing the converted and compressed .avi files
#' @param temp.overlay.folder  temporary directory to save the overlay subtitles (.ssa files)
#' @param overlay.folder directory where the overlay videos are saved
#' @param fps framerate in the original video
#' @param vid.length duration in seconds of the video
#' @param width width in pixels of the original videos
#' @param height height in pixels of the original video
#' @return returns nothing (NULL)
#' @export 

create.subtitle.overlays <- function(to.data, traj.data, raw.avi.folder, temp.overlay.folder, overlay.folder, fps, vid.length, width, height){
  
  #Define folder to store the temporary subtitles
  temp.overlays <- paste0(to.data, temp.overlay.folder)
  
  #Define folder where the input .avi files are stored
  input.avi.folder <- paste0(to.data, raw.avi.folder)
  
  #Define in the filtered trajectory data the start and end time of each observation, as well as the numeric ID of the observation
  traj.data$starttime <- (traj.data$frame)*(1/fps)
  traj.data$endtime <- traj.data$starttime + (1/fps)
  traj.data$ID.vid <- substr(traj.data$id, 14, nchar(traj.data$id))
  
  #Generate a subtitle line for each observation
  traj.data$subtitle <- paste0("Dialogue: 0,0:00:", sprintf("%05.2f", traj.data$starttime), ",0:00:", sprintf("%05.2f", traj.data$endtime), ",DefaultVCD,,0000,0000,0000,,{\\pos(", 
                               abs(round(traj.data$X)), ", ",abs(round(traj.data$Y)),")}", traj.data$ID.vid)
  
  #Generate the header for the subtitle file
  header <- c("[Script Info]", "ScriptType: v4.00+", "Collisions: Normal", paste0("PlayResX: ", width), paste0("PlayResY: ", height), 
              paste0("Timer: ", vid.length), "\r", "[V4+ Styles]", "Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding \rStyle: DefaultVCD,Arial,24,65535,65535,65535,65535,-1,0,0,0,100,100,0,0.00,1,1,0,2,0,0,0,0", "\r", "[Events]Format: Layer, Start, End, Style, Actor, MarginL, MarginR, MarginV, Effect, Text")
  
  #Make the folder to store the subtitle files, and generate the subtitle file for each file with observed cells 
  dir.create(paste0(temp.overlays), showWarnings = F)
  for (i in unique(traj.data$file)){
    lines <- append(header, unlist(traj.data[which(traj.data$file==i), "subtitle"]))
    data.table::fwrite(list(lines), file = paste0(temp.overlays, "/", i, ".ssa"), quote = F)
  }
  
  #Get a list of the subtitle files and the .avi files
  avi.files <- list.files(input.avi.folder, pattern = "\\.avi", full.names = T)
  ssa.files <- list.files(temp.overlays, pattern = "\\.ssa", full.names = T)
  
  #Create a folder to store the overlay videos
  dir.create(paste0(to.data, overlay.folder), showWarnings = F)
  
  #For each of the files with observed cells, burn the subtitles onto the avi file, and store the resulting file in the overlay folder
  for (i in unique(traj.data$file)){
    avi <- grep(i, avi.files, value = T)
    ssa <- grep(i, ssa.files, value = T)
    output <- paste0(to.data, overlay.folder, i, ".avi")
    system(paste0('ffmpeg -i ', avi, ' -vf "ass=', ssa, '" -b:v 50M -c:a copy -y ', output))
  }
  
  return(NULL)
}
