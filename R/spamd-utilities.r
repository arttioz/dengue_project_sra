##############################
#Utility functions for working
#with dengue data in Thailand
#in particular.
##############################



#' Function creates a cntry.data object for Thailand
#'
#'@param prov.data the data pn the provinces, their case counts, and their population in the year 2000
#'  assumed to have columns for names, code, FIPS, pop.2000 and every time, and rows for year, time.in.year, time
#'   and each province
#'@param wld.shape.file a shape file with FIPS_CNTRY and FIPS_ADMIN feilds.
#'
#'@return a complete cntry.data object.
#
load.thailand.dengue <- function(prov.data = "data/Dengue-Biweek-1968-2010-seed393.csv",
                                 wld.shape = "data/admin98/admin98.shp") {

    require(maptools)
    require(spatialpred)
    tmp.data <- read.csv(prov.data, as.is=T)
    world <- readShapeSpatial(wld.shape)
    thailand <- world[world$FIPS_CNTRY=="TH",]

    time.info <-  t(as.matrix(tmp.data[1:3,5:ncol(tmp.data)]))
    colnames(time.info)<-c("year","time.in.year","time")

    return(new.cntry.data(case.counts= as.matrix(tmp.data[4:nrow(tmp.data), 5:ncol(tmp.data)]),
                          time.info = time.info,
                          fips = tmp.data$FIPS[4:nrow(tmp.data)],
                          names =  tmp.data$names[4:nrow(tmp.data)],
                          pop.data = as.matrix(tmp.data$pop.2000[4:nrow(tmp.data)]),
                          loc.info = thailand))
}


#'Function creates a country object from a datbase connection. Right now restricts
#' cases to only DHF data
#'
#' @param credentials path to the credentials object
#' @param wld.shape link to the world shape object
#' @param columns which columns to pull
#' @param as.of.report if not null we subset to be before a reporting time. If NULL use latest
#' @param no.split.provinces if TRUE, all pre-1999 split provinces (both parents and children) are removed from data; if FALSE, only pre-1999 children are removed
#'
#' @return a cntry.data.linelist object
#'
load.thailand.dengue.db <- function (credentials,
                                     wld.shape = "data/updated_adm/ne_10m_admin_1_states_provinces.shp",
                                     columns = c("province", "delivery_date", "date_sick", "disease", "admission_status", "date_death",
                                         "outcome", "sex", "birth_year"),
                                     as.of.report=NULL,
                                     no.split.provinces=TRUE) {
    require(spatialpred)
    require(maptools)
    require(dplyr)
    require(lubridate)
    require(parallel)
    require(reshape)
    require(RPostgreSQL)    
    
    #make the link and get the line list data
    print("getting line list...")
    link <- db_connector(credentials)
    #query <- sprintf("SELECT %s FROM unique_case_data LIMIT 5;", paste(columns, collapse=","))
    query <- sprintf("SELECT %s FROM unique_case_data;", paste(columns, collapse=","))
    line.list.data <- dbGetQuery(link$conn, query)

    #print(line.list.data)

    #now get the aggregate counts..
    print("getting new case count data...")
    new_counts <-  import_case_counts(
        source_table = 'unique_case_data',
        group_by = c('disease','date_sick','province', 'delivery_date'),
        from_timepoint = now() - years(100), ## FROM (open)
        to_timepoint = now(), ## TO (closed)
        delivery_timepoint = now(),
        aggregate_formula = NULL,
        link = link
        )

    if (!is.null(as.of.report)) {
        new_counts <- new_counts[which(is.na(new_counts$delivery_date) | as.Date(new_counts$delivery_date)<=as.of.report),]
        line.list.data <- line.list.data[which(is.na(line.list.data$delivery_date) | as.Date(line.list.data$delivery_date)<=as.of.report),]
    }
    #print(tail(new_counts))

    #clean new counts
    new_counts$date_sick_biweek <- date_to_biweek(new_counts$date_sick)
    new_counts$date_sick_year <- year(new_counts$date_sick)

    #get old case counts
    print("getting old case count data...")

    old_counts <- import_old_case_counts(link=link)
    old_counts$delivery_date <- NA
    old_counts$date_sick <- NA

    #join old and new case count data
    print("Creating object...")
    counts <- joint_old_new_cases(new_counts, old_counts)

    ## keep only disease == 26
    counts <- filter(counts, disease==26);

    ##convert counts to a matrix
    counts$tm <- counts$date_sick_year * 100 + counts$date_sick_biweek
    case.mtrx <- cast(counts, province~tm, value="count")

    data(thai_prov_data)

    #should be updated
    world <- readShapeSpatial(wld.shape)
    #loc.info <- world[world$FIPS_CNTRY=="TH",]
    loc.info <- world[world$adm0_a3=="THA",]

    fips <- as.character(thai_prov_data$FIPS)

    shape.ind <- numeric(length(fips))


    ## for (i in 1:length(shape.ind)) {
    ##     if (fips[i]%in%as.character(loc.info$FIPS_ADMIN)) {
    ##         shape.ind[i] <- which(loc.info$FIPS_ADMIN==fips[i])
    ##     }
    ## }
   
    for (i in 1:length(shape.ind)) {
      if("gns_adm1" %in% names(loc.info@data)){
        shape.ind[i] <- which(loc.info@data$gns_adm1==fips[i])
      } else{
        #if (fips[i]%in%as.character(loc.info$fips)) {
            tmp <- which(loc.info$fips==fips[i])
            if (length(tmp)==1){
                #print(tmp[1])
                shape.ind[i] <- tmp[1]
            } else {
              tmp <- which(loc.info$fips_alt==fips[i])
                if (length(tmp)>0) {
                    #print(tmp[1])
                    shape.ind[i] <- tmp[1]
                } else {
                    tmp <- which(loc.info$fips==fips[i])
                    tmp2 <- which(is.na(loc.info$fips_alt[tmp]))
                    shape.ind[i] <- tmp[tmp2]
                }
            }
        #cat("*",shape.ind[i],"\n")
      }
    }
    #print(shape.ind)

    cnms <- colnames(case.mtrx)[-1]
    rwnms <-  rownames(case.mtrx)[-1]

    case.mtrx<-as.matrix(case.mtrx[,-1])

    ## time calculations
    t.step <- 1/26
    yr <- floor((as.numeric(cnms)-1)/100)
    time.in.yr <- as.integer((as.numeric(cnms))%%100)

    rc <- new("cntry.data.linelist",
              .Data = case.mtrx,
              n.locs = nrow(case.mtrx),
              n.times = ncol(case.mtrx),
              t = yr + (time.in.yr-1)*t.step,
              yr = yr,
              time.in.yr = time.in.yr,
              t.step = t.step,
              pop = as.matrix(thai_prov_data$Population),
              #t.step = 14/365.25, was doubly defined
              loc.info = loc.info[shape.ind,],
              mx.yr.time = 26,
              line.list = line.list.data)

    rc@names <- as.character(thai_prov_data$Province) #no idea why it breaks if you don't do it this way
    rownames(rc) <- rc@names
    #make sure NAs are only there for missing provinces.
    rc[is.na(rc)] <- 0 #get rid of all of the NAs
        if(no.split.provinces){
            rc["Sa Kaeo", which(rc@t<1999)] <- NA #weird, as province began in 1993
            rc["Prachin Buri", which(rc@t<1999)] <- NA #parent of Sa Kaeo; old pid TH45
            rc["Amnat Charoen", which(rc@t<1999)] <- NA #weird, as province began in 1993
            rc["Yasothon", which(rc@t<1972)] <- NA
            rc["Ubon Ratchathani", which(rc@t<1999)] <- NA #parent of Amnat Charoen and Yasothon; old pid TH71
            rc["Nong Bua Lam Phu", which(rc@t<1999)] <- NA
            rc["Udon Thani", which(rc@t<1999)] <- NA #parent of Nong Bua Lam Phu; old pid TH19
            rc["Mukdahan", which(rc@t<1999)] <- NA #weird, as province began in 1982
            rc["Nakhon Phanom", which(rc@t<1999)] <- NA #parent of Mukdahan; old pid TH21
            rc["Phayao", which(rc@t<1978)] <- NA
            rc["Chiang Rai", which(rc@t<1978)] <- NA #parent of Phayao
            rc["Nong Khai",] <- rc["Nong Khai",] + rc["Bueng Kan",] #add Bueng Kan counts to Nong Khai
            rc@.Data <- rc[-which(row.names(rc) == "Bueng Kan"),]
            rc@n.locs <- rc@n.locs - 1
            
            message("DO NOT SAVE THIS R OBJECT. IT MAY CONTAIN IDENTIFIABLE DATA.")
            message("All pre-split counts for split provinces have been marked as NA in the .Data object, except for Bueng Kan, which has been added to Nong Khai.")
        } else{
            rc["Sa Kaeo", which(rc@t<1999)] <- NA #weird, as province began in 1993
            rc["Amnat Charoen", which(rc@t<1999)] <- NA #weird, as province began in 1993
            rc["Yasothon", which(rc@t<1972)] <- NA
            rc["Ubon Ratchathani", which(rc@t<1972)] <- NA #parent of Amnat Charoen and Yasothon; old pid TH71
            rc["Nong Bua Lam Phu", which(rc@t<1999)] <- NA
            rc["Mukdahan", which(rc@t<1999)] <- NA #weird, as province began in 1982
            rc["Phayao", which(rc@t<1978)] <- NA
            rc["Chiang Rai", which(rc@t<1978)] <- NA #parent of Phayao
            rc["Nong Khai",] <- rc["Nong Khai",] + rc["Bueng Kan",] #add Bueng Kan counts to Nong Khai
            rc@.Data <- rc[-which(row.names(rc) == "Bueng Kan"),]
            rc@n.locs <- rc@n.locs - 1
            
            message("DO NOT SAVE THIS R OBJECT. IT MAY CONTAIN IDENTIFIABLE DATA.")
            message("Pre-split counts for split provinces have NOT been marked as NA in the .Data object, except for Bueng Kan, which has been added to Nong Khai.")
    }
    
    return(rc)

}


#'function to make credentials for dengue db
#' @param user the user name
#' @param password
#' @param port
#' @param fn file to save credentials
#'
#' @return file name where credentials are saved
#'
make.denguedb.credentials <- function(user, password, port=6393, fn="mycredentials.rds") {

    saveRDS(list(user=user, password=password, dbname="dengue_cases", host="localhost", port=port), file=fn)
    return(fn)

}




