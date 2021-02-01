extractFGP <- function(survey = NULL, path =NULL, data.dir = "c:/git/wrangledData/"){
  ts <-  format(Sys.time(), "%Y")

  if (is.null(path))path<-getwd()
  setwd(path)

  library(Mar.datawrangling)

  if (is.null(survey))survey <- c("SPRING", "SUMMER", "FALL", "4VSW")

  for (i in 1:length(survey)){
    fn <- paste0(survey[i],"_",ts)

    thisEnv <- new.env()
    months <- switch(survey[i],
                     "SPRING" = c(1,2,3,4),
                     "SUMMER" = c(5,6,7,8),
                     "FALL" = c(9,10,11,12),
                     "4VSW" = -1
    )

    Mar.datawrangling::get_data('rv', data.dir=data.dir, env=thisEnv, quiet = T)

    # valid tows
    thisEnv$GSXTYPE<-thisEnv$GSXTYPE[thisEnv$GSXTYPE$XTYPE==1,]

    if (survey[i] != "4VSW"){
      #get rid of 4VSW cod data
      thisEnv$GSINF <- thisEnv$GSINF[-which(thisEnv$GSINF$STRAT %in% c(396:411) & lubridate::month(thisEnv$GSINF$SDATE) %in% c(1,2,3,4)),]
      #retain appropriate months
      thisEnv$GSINF <- thisEnv$GSINF[which(lubridate::month(thisEnv$GSINF$SDATE) %in% months),]
    }else{
      thisEnv$GSINF <- thisEnv$GSINF[which(thisEnv$GSINF$STRAT %in% c(396:411) & lubridate::month(thisEnv$GSINF$SDATE) %in% c(1,2,3,4)),]
    }
    Mar.datawrangling::self_filter(env = thisEnv, quiet = T)


    Mar.utils::get_data_tables(schema = "GROUNDFISH", tables = c("GSGEAR","GSMATURITY","GSSEX"), data.dir=data.dir, env = thisEnv, usepkg = "roracle", quiet = T)
    # replace gear code with gear desc
    thisEnv$GSINF <- merge(thisEnv$GSGEAR, thisEnv$GSINF)
    # replace maturity code with maturity desc
    thisEnv$GSDET <- merge(thisEnv$GSDET, thisEnv$GSMATURITY, all.x = "T", by.x="FMAT", by.y="CODE")
    colnames(thisEnv$GSDET)[colnames(thisEnv$GSDET)=="DESCRIPTION"] <- "MATURITY"
    # replace sex code with sex desc
    thisEnv$GSDET <- merge(thisEnv$GSDET, thisEnv$GSSEX, all.x = "T", by.x="FSEX", by.y="CODE")
    colnames(thisEnv$GSDET)[colnames(thisEnv$GSDET)=="DESCRIPTION"] <- "SEX"

    #grab the depths (in fathoms).  if no value for DEPTH, average dmin and dmax, take the result, and convert to meters
    thisEnv$GSINF$DEPTH_M <- NA
    thisEnv$GSINF$DEPTH_M <- rowMeans(thisEnv$GSINF[,c("DMIN","DMAX")], na.rm = F) #first do average
    thisEnv$GSINF[!is.na(thisEnv$GSINF$DEPTH),"DEPTH_M"]<- thisEnv$GSINF[!is.na(thisEnv$GSINF$DEPTH),"DEPTH"] #overwrite w depth, where avail
    thisEnv$GSINF$DEPTH_M <- round(thisEnv$GSINF$DEPTH_M*1.8288,2) #convert to m
    
    # indicate and retain only desired fields
    GSMISSIONSflds  <- c("MISSION", 	"VESEL",	"CRUNO",	"YEAR",	"SEASON")
    GSINFflds       <- c("MISSION",	"SETNO",	"SDATE",	"TIME", "STRAT",	"LATITUDE",	"LONGITUDE",	"ELATITUDE",	"ELONGITUDE",	"DUR",	"DIST",	"SPEED",	"DEPTH_M",	"SURFACE_TEMPERATURE",	"BOTTOM_TEMPERATURE",	"BOTTOM_SALINITY", "GEARDESC")
    GSCATflds       <- c("MISSION",	"SETNO",	"SPEC",	"TOTWGT",	"TOTNO")
    GSDETflds       <- c("MISSION",	"SETNO",	"SPEC",	"FLEN",	"FWT", "MATURITY",	"SEX", "AGE",	"SPECIMEN_ID")
    GSSPECIESflds   <- c("SPEC",	"COMM",	"CODE",	"TSN")
    thisEnv$GSMISSIONS <- thisEnv$GSMISSIONS[,GSMISSIONSflds]
    thisEnv$GSINF <- thisEnv$GSINF[,GSINFflds]
    thisEnv$GSCAT <- thisEnv$GSCAT[,GSCATflds]
    thisEnv$GSDET <- thisEnv$GSDET[,GSDETflds]
    thisEnv$GSSPECIES <- thisEnv$GSSPECIES[,GSSPECIESflds]

    # notice that when adding time to date field, time can be offset by an hour (sd vs daylight savings?)
    # drop time from date
    thisEnv$GSINF$SDATE <- as.Date(thisEnv$GSINF$SDATE)
    # thisEnv$GSINF$YEAR<-lubridate::year(thisEnv$GSINF$SDATE)
    # thisEnv$GSINF$MONTH<-lubridate::month(thisEnv$GSINF$SDATE)
    # thisEnv$GSINF$DAY<-lubridate::day(thisEnv$GSINF$SDATE)
    # thisEnv$GSINF$HOUR <- as.integer(substr(sprintf('%04d',thisEnv$GSINF$TIME),1,2))
    # thisEnv$GSINF$MIN <- as.integer(substr(sprintf('%04d',thisEnv$GSINF$TIME),3,4))
    # thisEnv$GSINF$DATETIME <- lubridate::make_datetime(year = thisEnv$GSINF$YEAR, month = thisEnv$GSINF$MONTH, day = thisEnv$GSINF$DAY, hour =thisEnv$GSINF$HOUR, min = thisEnv$GSINF$MIN, sec=0, tz = "Canada/Atlantic")

    #rename fields as necess
    colnames(thisEnv$GSINF)[colnames(thisEnv$GSINF)=="SURFACE_TEMPERATURE"] <- "SURF_TEMP"
    colnames(thisEnv$GSINF)[colnames(thisEnv$GSINF)=="BOTTOM_TEMPERATURE"] <- "BOTT_TEMP"
    colnames(thisEnv$GSINF)[colnames(thisEnv$GSINF)=="BOTTOM_SALINITY"] <- "BOTT_SAL"
    colnames(thisEnv$GSINF)[colnames(thisEnv$GSINF)=="LATITUDE"] <- "SLAT"
    colnames(thisEnv$GSINF)[colnames(thisEnv$GSINF)=="LONGITUDE"] <- "SLONG"
    colnames(thisEnv$GSINF)[colnames(thisEnv$GSINF)=="ELATITUDE"] <- "ELAT"
    colnames(thisEnv$GSINF)[colnames(thisEnv$GSINF)=="ELONGITUDE"] <- "ELONG"
    colnames(thisEnv$GSINF)[colnames(thisEnv$GSINF)=="DEPTH_M"] <- "DEPTH"
    #  retain reasonable # of coord decimals
    thisEnv$GSINF$SLAT<- round(thisEnv$GSINF$SLAT,6)
    thisEnv$GSINF$ELAT<- round(thisEnv$GSINF$ELAT,6)
    thisEnv$GSINF$SLONG<- round(thisEnv$GSINF$SLONG,6)
    thisEnv$GSINF$ELONG<- round(thisEnv$GSINF$ELONG,6)
    
    # make the shapefile
    Mar.utils::df_to_shp(df=thisEnv$GSINF,filename = fn,lat.field = "SLAT",lon.field = "SLONG")

    write.csv(thisEnv$GSMISSIONS, file = paste0(fn,"GSMISSIONS.csv"), row.names = F)
    write.csv(thisEnv$GSINF, file = paste0(fn,"_GSINF.csv"), row.names = F)
    write.csv(thisEnv$GSCAT, file = paste0(fn,"_GSCAT.csv"), row.names = F)
    write.csv(thisEnv$GSDET, file = paste0(fn,"_GSDET.csv"), row.names = F)
    write.csv(thisEnv$GSSPECIES, file = paste0(fn,"_GSSPECIES.csv"), row.names = F)

    # toZip <- list.files(path = getwd(), pattern = paste0(fn, ".*(shp|shx|dbf|prj|csv)"))
    # zip::zipr(zipfile = paste0(fn,".zip"), files = toZip)
    # sapply(toZip, unlink)

  }
}

# extractFGP("SPRING")
# extractFGP("SUMMER")
# extractFGP("FALL")
# extractFGP("4VSW")
tt <- extractFGP(survey = NULL, path = "C:/Users/McMahonM/Documents/Assistance/FGP/20201109/")

