# loading required libraries
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(xml2))


if (Sys.info()[[4]] == "ANDRIUS-PC")
  setwd("D:/-=Works=-/R/GitHub/LTLABcollector")

if (Sys.info()[[4]] == "LTLAB-DEV")
  setwd("C:/R/GitHub/LTLABcollector")


refresh_rate <- 2
last_db_update_time <- 0
update_count <- 0


# choosing between real or testing tracedata file names
#tracedata_DB_name <- "//M85399/1_m85399/tracedata.db3"
tracedata_DB_name <- "tracedata.db3"

ltlab_smt_db <- "LTLAB_SMT_DB.sqlite"

current_project <- NULL
current_conveyor <- data.frame()

conn_status <- list(AX_DB_TBL = "NOT_OK",
                    SMT_DB = "NOT_OK",
                    SMT_PROJ = "NOT_OK",
                    PP_DATA = "NOT_OK",
                    UPDATE = "2015-01-01 01:00:00 EEST")


# functions saves current connectios status to csv file
status_to_csv = function() {
  data <- data.frame(AX_DB_TBL = conn_status$AX_DB_TBL,
                     SMT_DB = conn_status$SMT_DB,
                     SMT_PROJ = conn_status$SMT_PROJ,
                     PP_DATA = conn_status$PP_DATA)
  write.csv(data, "status.csv", row.names = FALSE)
}


# conveyorIDlist - list of the previous IDs in production, if some of them 
# changes it means they are finished and are writen to finished roduction 
# csv file with PCBfinishedtoCSV
conveyorIDlist <- c()
PCBfinished <- function(){
  conveyorIDlist <<- c(conveyorIDlist, conveyorID)
  conveyorIDlist <<- tail(conveyorIDlist, 100)
  if (length(conveyorIDlist) < 2){
    return()
  } else {
    finished <- setdiff(unlist(tail(conveyorIDlist, 2)[1]),
                        unlist(tail(conveyorIDlist, 2)[2]))
    if (length(finished) > 0) {
      PCBfinishedtoCSV(finished)
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
    fileName <- file.path("data", paste(date, ".csv", sep=""))
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

# checking if tracedata DB from production exists
if(file.exists(tracedata_DB_name)) {
  conn_status$AX_DB_TBL <- "OK"
  tracedata_DB <- src_sqlite(tracedata_DB_name)
} else {
  conn_status$AX_DB_TBL <- "NOT_OK"
  status_to_csv()
  Sys.sleep(refresh_rate)
  next()
}


if (last_db_update_time == 0) {
  tryCatch({
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
               status_to_csv()
               Sys.sleep(refresh_rate)
               next()
             }
  )
}


# checking if local SMT DB  exists
if(file.exists(ltlab_smt_db)) {
  tryCatch({
    smt_DB <- src_sqlite(ltlab_smt_db)
    projects <- tbl(smt_DB, sql("SELECT * FROM PROJEKTAI"))
    conn_status$SMT_DB <- "OK"
  }, error = function(e) {
               print("SMT DB is busy")
               conn_status$SMT_DB <- "NOT_OK"
               status_to_csv()
               Sys.sleep(refresh_rate)
               next()
             }
  )
} else {
  conn_status$SMT_DB <- "NOT_OK"
  status_to_csv()
  Sys.sleep(refresh_rate)
  next()
}


tryCatch({
  db_update_time <- file.info(tracedata_DB_name)$mtime
  if ((as.numeric(Sys.time()) - as.numeric(db_update_time) > 3600) && 
      (dim(current_conveyor)[1] == 0)) {
    current_project <- NULL
    Sys.sleep(refresh_rate)
    next()
  }
  db_update_time_diff <- as.numeric(db_update_time) - as.numeric(last_db_update_time)
  update_count <- c(update_count, db_update_time_diff) %>%
                  tail(2)
  if(update_count[1] > 0 && update_count[2] == 0){
    transportmap_new <- as.data.frame(
                          transportmap %>% 
                          filter(lPos %in% c(1:49)) %>%
                          select(idPCB, lPos, strPP) %>%
                          arrange(lPos) %>%
                          select(idPCB, lPos))
    current_conveyor_pcb_pos <- current_conveyor %>% 
                                select(idPCB, lPos)
    if(!identical(transportmap_new, current_conveyor_pcb_pos)) {
      current_conveyor <- as.data.frame(
                            transportmap %>% 
                            filter(lPos %in% c(1:49)) %>%
                            select(idPCB, lPos, strPP) %>%
                            arrange(lPos) %>%
                            mutate(lPos2 = lPos - 1.2))
      current_conveyor_id <- current_conveyor %>%
                             select(idPCB) %>%
                             as.data.frame()
      if (!dim(current_conveyor)[1] == 0) {
        current_project <- as.data.frame(current_conveyor %>%
                                         select(strPP))[1,]      
      }
    }
  }
  last_db_update_time <- db_update_time
}, error = function(e) {
             print("DB is busy 2")
             conn_status$AX_DB_TBL <- "NOT_OK"
             status_to_csv()
             Sys.sleep(refresh_rate)
             next()
           }
)

Sys.sleep(refresh_rate)
}


