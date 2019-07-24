#' @title getGeoData
#' @description This function can pull data down from a GeoData service on the 
#' DFO Geoportal site.  Tables with filters must be specified, and these 
#' determine what data is returned.
#' @param theService This specifies which service is being requested. The 
#' default is \code{NULL}, but the only current, valid value is 
#' \code{"FGP/Groundfish_WFS_Service"}
#' @param filt the default is \code{NULL}. This specifies the table(s) you want 
#' data from, and the appropriate values for any particular field(s).  The format 
#' is \code{tableName1.fieldName1:<value1,value2>&tableName1.fieldName2:<value1,value2>&tableName3.fieldName1:<value1,value2>} 
#' Please see the example for a real example.
#' @param extr_Lim default is \code{1000}.  This identifies how many records can 
#' be extracted in a single pull from the WFS service.  Unless you know that the 
#' value is something else, it should be left at 1000.
#' @author Mike McMahon 
#' @examples
#' getGeoData(theService="FGP/Groundfish_WFS_Service", filt="Groundfish_GSSPECIES.CODE:10,11,12&Groundfish_GSMISSIONS.YEAR:2015,2016,2017&Groundfish_GSMISSIONS.SEASON:SUMMER")
#' @family ArcGIS
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_name
#' @importFrom xml2 xml_children
#' @export
getGeoData<-function(theService=NULL, filt = NULL, extr_Lim = 1000){
  start = Sys.time()
  baseURL = paste0("http://geoportal.gc.ca/arcgis/services/",theService,"/GeoDataServer/WFSServer?SERVICE=WFS&VERSION=2.1.0")
  filts=list()
  filt=strsplit(filt,split = "&")[[1]]
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df) <- c("TABLE","FIELD","VALUES")
  for (f in 1:length(filt)){
    df[f,"TABLE"]<-sub('\\..*','',filt[f])
    df[f,"FIELD"]<-sub('.*\\.(.*):.*','\\1',filt[f])
    df[f,"VALUES"]<-sub('.*:','',filt[f])
  }
  fieldHandler<-function(fieldDF){
    TEMPL_filt = "<ogc:PropertyIsEqualTo>
  <ogc:PropertyName>::theField::</ogc:PropertyName>
  <ogc:Literal>::theValue::</ogc:Literal>
</ogc:PropertyIsEqualTo>"
    vals = strsplit(fieldDF$VALUES,split = ",")[[1]]
    if (length(vals)==1){
      this = gsub(pattern = "::theField::",replacement = fieldDF[,"FIELD"], x = TEMPL_filt)
      this = gsub(pattern = "::theValue::",replacement = fieldDF[,"VALUES"], x = this)
    }else{
      this = ""
      for (v in 1:length(vals)){
        thisv = gsub(pattern = "::theField::",replacement = fieldDF[,"FIELD"], x = TEMPL_filt)
        thisv = gsub(pattern = "::theValue::",replacement = vals[v], x = thisv)
        this = paste0(this, thisv)
      }
      this = paste0("<ogc:Or>",this,"</ogc:Or>")
    }
    return(this)
  } 
  tableHandler<-function(baseURL, tabDF, theService){
    TEMPL_url = paste0(baseURL, "&request=GetFeature&typeName=::theTable::")
    thisURL = gsub(pattern = "::theTable::",replacement = unique(tabDF$TABLE), x = TEMPL_url)
    chk = length(unique(c(tabDF$TABLE,tabDF$FIELD,tabDF$VALUES)))
    if (chk ==1)return(thisURL)
    u_f = unique(tabDF$FIELD)
    allFieldFilts = ""
    for (f in 1:length(u_f)){
      fieldDF = tabDF[tabDF$FIELD==u_f[f],]
      fieldFilt = fieldHandler(fieldDF)
      allFieldFilts = paste0(allFieldFilts, fieldFilt)
    }
    if (length(u_f)>1) allFieldFilts = paste0("<ogc:And>",allFieldFilts,"</ogc:And>")
    allFieldFilts = paste0("<ogc:Filter>",allFieldFilts,"</ogc:Filter>")
    FINAL_url = paste0(thisURL,"&FILTER='",URLencode(allFieldFilts),"'")
    return(FINAL_url) 
  } 
  
  getData <- function(url){
    svcName = sub('.*arcgis\\/services\\/(.*)\\/GeoDataServer.*','\\1',url)
    svcName = sub(pattern = "/",replacement = "_",x = svcName)
    if (length(grep(pattern = "FILTER=",x = url))>0){
      tblName = sub('.*&typeName=(.*)&FILTER=.*','\\1',url)
    }else{
      #No filter applied cause we're getting whole table
      tblName = sub('.*&typeName=(.*)','\\1',url)
    }
    if (nchar(url)>2000)stop("Your request generated a URL that is too long to be handled")
    cnt  <- tryCatch(
      {
        as.numeric(xml2::xml_attr(xml2::read_xml(paste0(url,"&f=pjson&resultType=hits")),"numberOfFeatures"))
      }
    )
    if(cnt>extr_Lim){
      rec_Start=0
      writeLines(paste0("\n",cnt," records exist.  Pulling ",extr_Lim," recs at a time."))
      pb = txtProgressBar(min=0, max=ceiling(cnt/extr_Lim), style = 3)
      for (i in 1:ceiling(cnt/extr_Lim)){
        if ((cnt-rec_Start) < extr_Lim){
          extr_Lim<-(cnt-rec_Start)
        }
        this_pull <- paste0(url,'&startIndex=',rec_Start,'&count=',extr_Lim)
        theseRecs = xml2::read_xml(this_pull)
        theseRecs <- tryCatch(
          {
            xml2::xml_find_all(theseRecs,paste0("//",svcName,":",tblName))
          },
          error=function(cond){
          }
        )
        if(length(theseRecs)<1)return(NULL)
        #create an empty dataframe with the possible column names
        names =  unique(xml2::xml_name(xml2::xml_children(theseRecs)))
        if (i ==1){
          df_master <- data.frame(matrix(ncol = length(names), nrow = 0))
          colnames(df_master) <- names
        }
        df_this = df_master[FALSE,]
        for (r in 1:length(theseRecs)){
          df_this[r,xml2::xml_name(xml2::xml_children(theseRecs[[r]]))]<-xml2::xml_text(xml2::xml_children(theseRecs[[r]]))
        }
        df_master = tryCatch(
          {
            rbind(df_master, df_this) 
          },
          error=function(cond){
            #can't rbind to initial df suggests that a new field was encountered
            #find the new field, add it to master and set it to NA prior to 
            #trying the rbind again.
            if (length(setdiff(names(df_this),names(df_master)))>0){
              newFields = setdiff(names(df_this),names(df_master))
              df_master[newFields]<-NA
              rbind(df_master, df_this) 
            }
          }
        )
        rec_Start = rec_Start+extr_Lim
        Sys.sleep(0.1)
        setTxtProgressBar(pb, i)
      }
      close(pb)
    }else{
      results = xml2::read_xml(url)
      cat("\ngot the xml")
      allRecs <- tryCatch(
        {
          xml2::xml_find_all(results,paste0("//",svcName,":",tblName))
        },
        error=function(cond){
        }
      )
      if(length(allRecs)<1)return(NULL)
      #create an empty dataframe with the possible column names
      names =  unique(xml2::xml_name(xml2::xml_children(allRecs)))
      df_master <- data.frame(matrix(ncol = length(names), nrow = 0))
      colnames(df_master) <- names
      #fill in the dataframe row by row
      for (r in 1:length(allRecs)){
        df_master[r,xml2::xml_name(xml2::xml_children(allRecs[[r]]))]<-xml2::xml_text(xml2::xml_children(allRecs[[r]]))
      }
    }
    res=list(tblName,df_master)
    return(res)
  }
  
  u_t = unique(df$TABLE)
  allTableFilts=list()
  
  for (t in 1:length(u_t)){
    thisTabDF = df[df$TABLE==u_t[t],]
    thisTableQuery = tableHandler(baseURL, thisTabDF, theService)
    allTableFilts[[t]]=thisTableQuery
  }
  data=list()
  for (u in 1:length(allTableFilts)){
    this = getData(allTableFilts[[u]])
    if (is.null(this))return(NULL)
    data[[this[[1]]]]<-this[[2]]
    rm(this)  
    # pb <- txtProgressBar(min = 0, max = length(allTableFilts), style = 3)
    # setTxtProgressBar(pb, u)
  }
  cat(paste0("\nCompleted in ",format(.POSIXct(difftime(Sys.time(), start, units="secs"),tz="GMT"), "%H:%M:%S"),"\n"))
  return(data)
}