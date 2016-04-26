# loading required libraries
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(xml2))


if (Sys.info()[[4]] == "ANDRIUS-PC")
  setwd("D:/-=Works=-/R/GitHub/LTLABcollector")

if (Sys.info()[[4]] == "LTLAB-DEV")
  setwd("C:/R/GitHub/LTLABcollector")


refresh_rate <- 3
last_db_update_time <- 0
update_count <- 0


# choosing between real or testing tracedata file names
#tracedata_DB_name <- "//M85399/1_m85399/tracedata.db3"
tracedata_DB_name <- "tracedata.db3"

ltlab_smt_db <- "LTLAB_SMT_DB.sqlite"

current_project <- NULL
last_project <- "Noname"
current_conveyor <- data.frame()

conn_status <- list(AX_DB_TBL = "NOT_OK",
                    SMT_DB = "NOT_OK",
                    SMT_PROJ = "NOT_OK",
                    PP_DATA = "NOT_OK")
last_status <- data.frame()

# functions saves current connectios status to csv file
status_to_csv <- function(date = 0) {
  data <- data.frame(AX_DB_TBL = conn_status$AX_DB_TBL,
                     SMT_DB = conn_status$SMT_DB,
                     SMT_PROJ = conn_status$SMT_PROJ,
                     PP_DATA = conn_status$PP_DATA)
  last_status <- data.frame(AX_DB_TBL = last_status$AX_DB_TBL,
                            SMT_DB = last_status$SMT_DB,
                            SMT_PROJ = last_status$SMT_PROJ,
                            PP_DATA = last_status$PP_DATA)
  if (!identical(data, last_status)) {
    last_status <<- data
    data["Update"] <- date
    print("Status change:")
    print(data)
    write.csv(data, "status.csv", row.names = FALSE)
  }
}
status_to_csv(date = Sys.time())

# conveyorIDlist - list of the previous IDs in production, if some of them 
# changes it means they are finished and are writen to finished roduction 
# csv file with PCBfinishedtoCSV
conveyorIDlist <- c()
PCBfinished <- function(){
  conveyorIDlist <- c(conveyorIDlist, current_conveyor_ids) %>%
                    tail(2)
  conveyorIDlist <<- conveyorIDlist
  if (length(conveyorIDlist) < 2){
    return()
  } else {
    finished <- setdiff(unlist(conveyorIDlist[1]),
                        unlist(conveyorIDlist[2]))
    print(paste("Finished: ", finished))
    if (length(finished) > 0) {
      tryCatch({
        PCBfinishedtoCSV(finished)
        print("All data writen!!!!")
      }, error = function(e) {
        print(e)
        status_to_csv(date = Sys.time())
        next()
      })
    }
  }
}


# function to write data of finished PCBs to csv file;
# name of the file - current date
PCBfinishedtoCSV <- function(idlist) {
  for (id in idlist) {
    proj <- current_project
    date <- format(Sys.time(), format = "%Y-%m-%d")
    time <- strftime(Sys.time(), format="%H:%M:%S")
    PCBquant <- current_project_PCB_quantity
    PCBcomp <- current_project_comp_quantity
    fileName <- file.path("data", paste(date, "csv", sep = "."))
    all <- data.frame(ID = id,
                      PROJECT = proj,
                      DATE = date,
                      TIME = time,
                      QUANTITY = PCBquant,
                      COMPONENTS = PCBcomp,
                      row.names = NULL, 
                      stringsAsFactors = FALSE)
    if (file.exists(fileName)) {
      conn_status$UPDATE <- time
      write.table(all,
                  file = fileName, 
                  sep = ",",
                  append = TRUE,
                  row.names = FALSE,
                  col.names = FALSE)
    } else {
      conn_status$UPDATE <- time
      write.table(all,
                  file = fileName, 
                  sep = ",",
                  row.names = FALSE,
                  col.names = TRUE)      
    }
  }
}


# next section needs to be looped forever -------------------------------------
while (TRUE) {
Sys.sleep(refresh_rate)
print("-----------------------------------------------------")

system.time({

# checking if tracedata DB from production exists
if(file.exists(tracedata_DB_name)) {
  conn_status$AX_DB_TBL <- "OK"
  tracedata_DB <- src_sqlite(tracedata_DB_name)
  print("AX_DB_TBL - OK")
} else {
  conn_status$AX_DB_TBL <- "NOT_OK"
  status_to_csv(date = Sys.time())
  print("AX_DB_TBL - NOT OK!!!!")
  next()
}


if (last_db_update_time == 0) {
  tryCatch({
    print("First update")
    last_db_update_time <- file.info(tracedata_DB_name)$mtime
    transportmap <- tbl(tracedata_DB, sql("SELECT * FROM transportmap"))
    current_conveyor <- as.data.frame(
                          transportmap %>% 
                          filter(lPos %in% c(1:49)) %>%
                          select(idPCB, lPos, strPP) %>%
                          arrange(lPos) %>%
                          mutate(lPos2 = lPos - 1.2))
    if (!dim(current_conveyor)[1] == 0) {
      current_project <- as.data.frame(current_conveyor %>%
                                       select(strPP))[1,]      
    }
  }, error = function(e) {
               print("DB is busy 1")
               conn_status$AX_DB_TBL <- "NOT_OK"
               status_to_csv(date = Sys.time())
               next()
             }
  )
}


# checking if local SMT DB  exists
if(file.exists(ltlab_smt_db)) {
  tryCatch({
    smt_DB <- src_sqlite(ltlab_smt_db)
    projects <- tbl(smt_DB, sql("SELECT * FROM PROJEKTAI"))
    all_projects_names <- projects %>% select(PROJEKTAS)
    conn_status$SMT_DB <- "OK"
    print("SMT_DB - OK")
  }, error = function(e) {
               print("SMT DB is busy")
               conn_status$SMT_DB <- "NOT_OK"
               status_to_csv(date = Sys.time())
               next()
             }
  )
} else {
  conn_status$SMT_DB <- "NOT_OK"
  status_to_csv(date = Sys.time())
  print("SMT_DB - NOT OK!!!!")
  next()
}

print(paste("Current project:", current_project))
  
tryCatch({
  db_update_time <- file.info(tracedata_DB_name)$mtime
  
  if ((as.numeric(Sys.time()) - as.numeric(db_update_time) > 120) && 
      (dim(current_conveyor)[1] == 0)) {
    current_project <- "No production"
    conn_status$SMT_PROJ <- "NOT_OK"
    conn_status$PP_DATA <- "NOT_OK"
    if (last_project != current_project) {
      current_project_data <- data.frame(current_project = NA,
                                         current_project_name = NA,
                                         current_project_side = NA,
                                         current_project_comp_quantity = NA,
                                         current_project_PCB_quantity = NA,
                                         CycleTime = NA,
                                         PCBLength = NA,
                                         PCBWidth = NA,
                                         BoardPitch = NA,
                                         MaximumNumberOfBoards = NA,
                                         StopperInX = NA)
      
      print(current_project_data)
      write.csv(current_project_data, 
                "current_project_data.csv", 
                row.names = FALSE)
      last_project <- current_project
    }
    print("No production!!!!")
    status_to_csv(date = Sys.time())
    next()
  }
  
  db_update_time_diff <- as.numeric(db_update_time) - as.numeric(last_db_update_time)
  last_db_update_time <- db_update_time
  update_count <- c(update_count, db_update_time_diff) %>%
                  tail(2)
  if(update_count[1] > 0 && update_count[2] == 0){
    print("DB update found")
    transportmap_new <- as.data.frame(
                          transportmap %>% 
                          filter(lPos %in% c(1:49)) %>%
                          select(idPCB, lPos, strPP) %>%
                          arrange(lPos) %>%
                          select(idPCB, lPos))
    current_conveyor_pcb_pos <- current_conveyor %>% 
                                select(idPCB, lPos)
    if(!identical(transportmap_new, current_conveyor_pcb_pos)) {
      print("Transport IDs changed!")
      current_conveyor <- as.data.frame(
                            transportmap %>% 
                            filter(lPos %in% c(1:49)) %>%
                            select(idPCB, lPos, strPP) %>%
                            arrange(lPos) %>%
                            mutate(lPos2 = lPos - 1.2))
      current_conveyor_ids <- current_conveyor %>%
                              select(idPCB) %>%
                              as.data.frame()
      if (!dim(current_conveyor)[1] == 0) {
        current_project <- as.data.frame(current_conveyor %>%
                                         select(strPP))[1,]      
      }
      write.csv(current_conveyor, 
                "current_conveyor.csv", 
                row.names = FALSE)
      print(current_conveyor)
    } else {
      print("No transport change!!!!")
      status_to_csv(date = Sys.time())
      next()
    }
  } else {
    print("No DB update!!!!")
    status_to_csv(date = Sys.time())
    next()
  }
}, error = function(e) {
             print("DB is busy 2")
             conn_status$AX_DB_TBL <- "NOT_OK"
             status_to_csv(date = Sys.time())
             next()
           }
)


if (last_project != current_project) {
  tryCatch({
    current_project_name <- current_project %>% 
                            sub("_BOT","",.) %>% 
                            sub("_TOP","",.)
    current_project_side <- ifelse(grepl("_BOT$", current_project),
                                   c("BOT"),
                                   c("TOP"))
    all_names <- all_projects_names %>% 
                 as.data.frame() %>% 
                 .[["PROJEKTAS"]]
    
    if (current_project_name %in% all_names) {
      current_project_comp_quantity <- projects %>%
                                       filter(PROJEKTAS == current_project_name) %>%
                                       select(ifelse(current_project_side == "TOP",
                                                     TOP_KOMP,
                                                     BOT_KOMP)) %>% 
                                       as.data.frame() %>% 
                                       as.numeric()
      conn_status$SMT_PROJ <- "OK"
      print(paste("SMT_PROJ - OK - ", current_project_comp_quantity))
    } else {
      print("SMT_PROJ - NOT OK!!!!")
      conn_status$SMT_PROJ <- "NOT_OK"
      status_to_csv(date = Sys.time())
      next()
    }

    pp_file_name <- file.path("PP", 
                              paste(current_project, "PP", sep = "."))

    if (file.exists(pp_file_name)) {
      PnPFileXML <- read_xml(pp_file_name)
      
      CycleTime <-  xml_find_all(PnPFileXML, ".//CycleTime") %>%
                    xml_text() %>%
                    as.numeric()

      PCBLength <-  xml_find_all(PnPFileXML, ".//PCBLength") %>%
                    xml_text() %>%
                    as.numeric()

      PCBWidth <- xml_find_all(PnPFileXML, ".//PCBWidth") %>%
                  xml_text() %>%
                  as.numeric()               

      BoardPitch <- xml_find_all(PnPFileXML, ".//BoardPitch") %>%
                    xml_text() %>%
                    as.numeric() 

      MaximumNumberOfBoards <- xml_find_all(PnPFileXML, ".//MaximumNumberOfBoards") %>%
                               xml_text() %>%
                               as.numeric()

      StopperInX <- xml_find_all(PnPFileXML, ".//StopperInX") %>%
                    xml_text() %>%
                    as.numeric()                         

      current_project_PCB_quantity <- xml_find_all(PnPFileXML, ".//CircuitId") %>%
                                      xml_text() %>%
                                      as.numeric() %>%
                                      max()
      
      current_project_data <- data.frame(current_project,
                                         current_project_name,
                                         current_project_side,
                                         current_project_comp_quantity,
                                         current_project_PCB_quantity,
                                         CycleTime,
                                         PCBLength,
                                         PCBWidth,
                                         BoardPitch,
                                         MaximumNumberOfBoards,
                                         StopperInX)
      
      print(current_project_data)
      write.csv(current_project_data, 
                "current_project_data.csv", 
                row.names = FALSE)
      conn_status$PP_DATA <- "OK"
      print("PP_DATA - OK")
    } else {
      print("PP_DATA - NOT OK!!!!")
      conn_status$PP_DATA <- "NOT_OK"
      status_to_csv(date = Sys.time())
      next()
    }   

    last_project <- current_project
  }, error = function(e) {
    print("SMT DB is busy 2")
    conn_status$SMT_PROJ <- "NOT_OK"
    status_to_csv(date = Sys.time())
    next()
  })  
}

PCBfinished()


status_to_csv(date = Sys.time())
print("Cycle finished!!!!!!!!!!!!!!!!")
}) -> timing

print(timing)
}


