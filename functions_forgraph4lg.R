graphab_importdem<-function (proj_name,proj_path = NULL, alloc_ram = NULL, demraster = NULL)
{
  if(is.null(demraster)){
    print("Dem raster not found")
  }
  proj_end_path <- paste0(proj_name, "/", proj_name, 
                          ".xml")
  
  gr <- get_graphab(res = FALSE, return = TRUE)
  if (gr == 1) {
    message("Graphab has been downloaded")
  }
  java.path <- Sys.which("java")
  version <- "graphab-2.6.jar"
  path_to_graphab <- paste0(rappdirs::user_data_dir(), "/graph4lg_jar/", 
                            version)
  cmd <- c("-Djava.awt.headless=true", "-jar", 
           path_to_graphab, "--project", proj_end_path)
  
  cmd2<-c("-Djava.awt.headless=true", "-jar", path_to_graphab, "--project", proj_end_path,"--dem",demraster)
  
  rs<-system2(java.path, args = cmd2, stdout = TRUE)
}

graphab_link<-function (proj_name, distance = "cost", name, cost = NULL, 
                        topo = "planar", proj_path = NULL, alloc_ram = NULL, rasterfile=NULL,maxcost=NULL,slopecoef = NULL)
{
  if (!is.null(proj_path)) {
    chg <- 1
    wd1 <- getwd()
    setwd(dir = proj_path)
  }else {
    chg <- 0
    proj_path <- getwd()
  }
  if (!inherits(proj_name, "character")) {
    if (chg == 1) {
      setwd(dir = wd1)
    }
    stop("'proj_name' must be a character string")
  }else if (!(paste0(proj_name, ".xml") %in% list.files(path = paste0("./", 
                                                                      proj_name)))) {
    if (chg == 1) {
      setwd(dir = wd1)
    }
    stop("The project you refer to does not exist.\n         Please use graphab_project() before.")
  }
  proj_end_path <- paste0(proj_name, "/", proj_name, 
                          ".xml")
  if (!inherits(distance, "character")) {
    if (chg == 1) {
      setwd(dir = wd1)
    }
    stop("'distance' must be a character string")
  }else if (!(distance %in% c("cost", "euclid"))) {
    if (chg == 1) {
      setwd(dir = wd1)
    }
    stop("'distance' must be equal to 'cost' or 'euclid'")
  }
  if (distance == "cost") {
    if (!inherits(cost, "data.frame")) {
      if (chg == 1) {
        setwd(dir = wd1)
      }
      stop("'cost' must be a data.frame object")
    }else {
      if (!all(c("code", "cost") %in% colnames(cost))) {
        if (chg == 1) {
          setwd(dir = wd1)
        }
        stop("The columns of cost must include 'code' and 'cost'")
      }else if (any(is.na(as.numeric(cost$code)))) {
        if (chg == 1) {
          setwd(dir = wd1)
        }
        stop("'code' column must include numeric values")
      }else if (any(is.na(as.numeric(cost$cost)))) {
        if (chg == 1) {
          setwd(dir = wd1)
        }
        stop("'cost' column must include numeric values")
      }
      if (inherits(cost$code, c("factor", "character"))) {
        cost$code <- as.numeric(as.character(cost$code))
      }
      if (inherits(cost$cost, c("factor", "character"))) {
        cost$cost <- as.numeric(as.character(cost$cost))
      }
      rast_codes <- graph4lg::get_graphab_raster_codes(proj_name = proj_name, 
                                                       mode = "all")
      if (!all(rast_codes %in% cost$code)) {
        if (chg == 1) {
          setwd(dir = wd1)
        }
        stop("'code' column must include all the raster code values.")
      }
    }
    ncode <- nrow(cost)
    vec_cost <- c()
    for (i in 1:ncode) {
      vec_cost <- c(vec_cost, paste0(cost[i, "code"], 
                                     "=", cost[i, "cost"]))
    }
  } else if (!is.null(cost)) {
    message("'cost' argument is ignored with 'distance = euclid'")
  }
  if (!inherits(name, "character")) {
    if (chg == 1) {
      setwd(dir = wd1)
    }
    stop("'name' must be a character string")
  }
  gr <- get_graphab(res = FALSE, return = TRUE)
  if (gr == 1) {
    message("Graphab has been downloaded")
  }
  java.path <- Sys.which("java")
  version <- "graphab-2.6.jar"
  path_to_graphab <- paste0(rappdirs::user_data_dir(), "/graph4lg_jar/", 
                            version)
  cmd <- c("-Djava.io.tmpdir=C:\\Users\\nharisena\\Documents\\research\\Working\\temp","-Djava.awt.headless=true", "-jar", 
           path_to_graphab, "--project", proj_end_path, "--linkset", 
           paste0("distance=", distance), paste0("name=", 
                                                 name))
  if (topo == "complete") {
    cmd <- c(cmd, "complete")
  }
  
  
  if (distance == "cost") {
    #cmd <- c(cmd, vec_cost)
  }
  
  
  if (!is.null(alloc_ram)) {
    if (inherits(alloc_ram, c("integer", "numeric"))) {
      
      
      cmd <- c(paste0("-Xmx25g"), 
               cmd)
      
      #cmd <- c(paste0("-Xmx", alloc_ram, "g"), 
      #cmd)
    }
    else {
      if (chg == 1) {
        setwd(dir = wd1)
      }
      stop("'alloc_ram' must be a numeric or an integer")
    }
  }
  #rasterfile<-paste("C:\\Users\\nharisena\\Documents\\research\\Working\\firsttrywithdata\\2010\\nodes\\shapefile\\H_Res_mittelland_trynew3_extcost.tif")
  
  if(!is.null(rasterfile)){
  cmd <- c(cmd,paste0("extcost=",rasterfile) )}
  
  if(!is.null(maxcost)){
  cmd <- c(cmd,paste0("maxcost=",maxcost) )}
  
  if(!is.null(slopecoef)){
    cmd <- c(cmd,paste0("slope=",slopecoef) )}
  
  cmd<-c(cmd,"xStream.ignoreUnknownElements()")
  
  #demraster<-paste("C:\\Users\\nharisena\\Documents\\research\\Working\\firsttrywithdata\\2010\\Resistance_surface\\Slope_mtlnd.tif")
  
  #cmd2<-c("-Djava.awt.headless=true", "-jar", path_to_graphab, "--project", proj_end_path,"--dem",demraster)
  
  rs <- try(system2(java.path, args = cmd, stdout = TRUE))
  print(rs)
  #rs2<-try(system2(java.path, args = cmd2, stdout = TRUE))
  
  
  
  if (chg == 1) {
    setwd(dir = wd1)
  }
  if (length(rs) == 1) {
    if (rs == 1) {
      message("An error occurred")
    }
  }
  else {
    message(paste0("Link set '", name, "' has been created in the project ", 
                   proj_name))
  }
}
