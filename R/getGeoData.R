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
#' @author Mike McMahon 
#' @examples
#' getGeoData(theService="FGP/Groundfish_WFS_Service", filt="Groundfish_GSSPECIES.CODE:10,11,12&Groundfish_GSMISSIONS.YEAR:2015,2016,2017&Groundfish_GSMISSIONS.SEASON:SUMMER")
#' @family ArcGIS
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_name
#' @importFrom xml2 xml_children
#' @export
getGeoData<-function(theService=NULL, filt = NULL){
  start = Sys.time()
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
  
  tableHandler<-function(tabDF, theService){
    TEMPL_url = paste0("http://geoportal.gc.ca/arcgis/services/",theService,"/GeoDataServer/WFSServer?SERVICE=WFS&VERSION=1.1.0&request=GetFeature&typeName=::theTable::")
    thisURL = gsub(pattern = "::theTable::",replacement = unique(tabDF$TABLE), x = TEMPL_url)
    u_f = unique(tabDF$FIELD)
    if ("ALL" %in% u_f){
      FINAL_url = thisURL
    }else{
      allFieldFilts = ""
      for (f in 1:length(u_f)){
        fieldDF = tabDF[tabDF$FIELD==u_f[f],]
        fieldFilt = fieldHandler(fieldDF)
        allFieldFilts = paste0(allFieldFilts, fieldFilt)
      }
      if (length(u_f)>1) allFieldFilts = paste0("<ogc:And>",allFieldFilts,"</ogc:And>")
      allFieldFilts = paste0("<ogc:Filter>",allFieldFilts,"</ogc:Filter>")
      FINAL_url = paste0(thisURL,"&FILTER='",URLencode(allFieldFilts),"'")
    }
    return(FINAL_url) 
  } 
  u_t = unique(df$TABLE)
  allTableFilts=list()
  
  for (t in 1:length(u_t)){
    thisTabDF = df[df$TABLE==u_t[t],]
    thisTableQuery = tableHandler(thisTabDF, theService)
    allTableFilts[[t]]=thisTableQuery
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
        as.numeric(xml2::xml_attr(xml2::read_xml(paste0(url,"&resultType=hits")),"numberOfFeatures"))
      }
    )
    if(cnt>1000){
      extr_Lim = 1000 #can get this many at a time
      #&resultType=hits
      #count=N&
      #  sortBy=attribute
      #&startIndex=0&count=5
      #GSCAT =  213038
      #GSDET = 2056813
      cat("\nMore than 100 recs - multi-pull starting")
      pb = txtProgressBar(min=0,
                          max=ceiling(cnt/extr_Lim),
                          style = 3)
      rec_Start=0
      df = data.frame()
      for (i in 1:2){ #cnt){ #ceiling(cnt/extr_Lim)){
        if ((cnt-rec_Start) < extr_Lim){
          extr_Lim<-(cnt-rec_Start)
        }
        
        this_pull <- paste0(url,"&startIndex=",rec_Start,"&count=",extr_Lim)
        # print(this_pull)
        #browser()
        allRecs_this <- xml2::read_xml(this_pull)
        names =  unique(xml2::xml_name(xml2::xml_children(xml2::xml_children(xml2::xml_children(allRecs_this)))))
        thisPullDF <- data.frame(matrix(ncol = length(names), nrow = 0))
        
        colnames(thisPullDF) <- names
        for (r in 1:length(allRecs_this)){
          thisPullDF[r,xml2::xml_name(xml2::xml_children(xml2::xml_children(xml2::xml_children(allRecs_this))[r]))]<-xml2::xml_text(xml2::xml_children(xml2::xml_children(xml2::xml_children(allRecs_this))[r]))
        }
        rec_Start = rec_Start+extr_Lim
        df = rbind(df,thisPullDF)
        Sys.sleep(0.1)
        setTxtProgressBar(pb, i)
      }
      close(pb)
      
    }else{
      
      
      results = xml2::read_xml(url)
      print("\ngot the xml")
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
      df <- data.frame(matrix(ncol = length(names), nrow = 0))
      colnames(df) <- names
      
      #fill in the dataframe row by row
      for (r in 1:length(allRecs)){
        df[r,xml2::xml_name(xml2::xml_children(allRecs[[r]]))]<-xml2::xml_text(xml2::xml_children(allRecs[[r]]))
      }
      
    }
    res=list(tblName,df)
    return(res)
  }
  data=list()
  
  for (u in 1:length(allTableFilts)){
    this = getData(allTableFilts[[u]])
    if (is.null(this))return(NULL)
    data[[this[[1]]]]<-this[[2]]
    rm(this)  
    #pb <- txtProgressBar(min = 0, max = length(allTableFilts), style = 3)
    # setTxtProgressBar(pb, u)
  }
  cat(paste0("\nCompleted in ",format(.POSIXct(difftime(Sys.time(), start, units="secs"),tz="GMT"), "%H:%M:%S"),"\n"))
  return(data)
}