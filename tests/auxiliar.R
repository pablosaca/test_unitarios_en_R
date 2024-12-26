library(stringr)

#************************************************************
#*************** APLICACIÓN DE TEST UNITARIOS ***************
#************************************************************

# ------------- funciones para obtener la ruta del fichero -------------
get_path <- function(){
  
  #--------------------------------------
  # Ruta básica de los archivos
  #--------------------------------------
  
  # salida:
  #  default_path (string)
  
  default_path <- getwd()
  elem_default = "/"
  elem_path <- unlist(str_split(default_path, elem_default))
  
  default_path <- paste0(elem_path[[1]],
                         elem_default,
                         elem_path[[2]],
                         elem_default,
                         elem_path[[3]],
                         elem_default,
                         elem_path[[4]],
                         elem_default,
                         "Escritorio",
                         elem_default
  ) 
  
  return(default_path)
}


get_source <- function(folder_path, file_path){
  
  #--------------------------------------
  # Ruta específica de los archivos
  #--------------------------------------
  
  # entrada:
  #  folder_path (string)
  #  file_path (string)
  # salida:
  #  default_path (string)
  
  default_path <- get_path()
  
  elem_default = "/"
  format_file = ".R"
  
  path <- paste0(
    default_path,
    folder_path,
    elem_default,
    file_path,
    format_file
  )
  
  return(path)
}
