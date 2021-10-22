barplot_detection_frequency_chem <- function(df_detect_freq
                                             , current_directory)
{

  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Define the name of the new folder to contain the alphabet soup plots
  name_folder <- "Barplot - Detection Frequency by Chemical"
  
  # Make a new folder if the folder doesn't exist
  if(name_folder %in% all_files_in_current_directory)
  {
    
  } else {
    dir.create(name_folder)
  }
  
  # Define a string for the working directory for the new folder
  new_working_directory <- paste(current_directory
                                 , name_folder
                                 , sep = "/")
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  chem_family_colors <- c("#8B0000"    # Acrylamide
                          , "#33FFC7"  # Aldehydes
                          , "#334FFF"   # Aromatic Amines
                          , "#EE0000"    # BFRs
                          , "#CEFF33"    # Dietary Components
                          , "#FFA500"    # Dioxins
                          , "#EEEE00"    # Furans
                          , "#228B22"    # Metals
                          , "#BABABA"    # Other
                          , "#FFB6C1"    # PFCs
                          , "#A2CD5A"    # Personal Care
                          , "#1E90FF"    # Pesticides
                          , "#FF6B00"    # PFRs
                          , "#A4D3EE"    # Phthalates & Plasticizers
                          , "#7D26CD"    # Phytoestrogens
                          , "#FFBBFF"    # PAHs
                          , "#FF69B4"    # PCBs
                          , "#8B4513"    # Smoking
                          , "#828282"    # VOCs
                          )
  
  barplot <- ggplot(data = df_detect_freq
                    , aes(x = reorder(chemical_name, -perc_above_LOD)
                          , y = perc_above_LOD
                          , fill = chem_family)) +
    geom_bar(stat = "identity") +
    xlab("Chemicals") +
    ylab("Percentage of Measurements above LOD") +
    scale_fill_manual(values = chem_family_colors) +
    theme(axis.text.x = element_text(angle = 90
                                     , hjust = 1
                                     , size = 4)
          , legend.position = "top"
          , legend.text = element_text(size = 9)
          )
  
  plot_name.png <- paste("barplot_detection_frequency"
                         ,".png"
                         , sep = "")
  plot_name.pdf <- paste("barplot_detection_frequency"
                         ,".pdf"
                         , sep = "")
  
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = barplot
         , width = 14
         , height = 9)
  
  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = barplot
         , width = 14
         , height = 9)
  
  # Set the working directory back to the main directory 
  setwd(current_directory)
}
 