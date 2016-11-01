#' assets return demo dataset.
#'
#' A dataset containing stock index and bond index daily return data since 2009.
#'
#' @format A data frame with 1627 rows and 3 variables:
#' \describe{
#'   \item{date}{date type}
#'   \item{stock}{stock index return}
#'   \item{bond}{bond index return}
#' }
"rtndemo"


#' ladder position port function
#'
#' @param assetRtn a data frame for stock and bond return.
#' @param ruledf a data frame for position rule.
#' @param rebalance rebalance frequency, default value is NULL.
#' @return a data frame, containing return,postion,nav.
#' @export
#' @examples
#' assetRtn <- rtndemo
#' ruledf <- data.frame(nav = c(0,1.05,1.10,1.20),pos = c(0.15,0.25,0.35,0.5))
#' re <- ladderNAV(assetRtn,ruledf)
#' rebalance <- '2 years'
#' re <- ladderNAV(assetRtn,ruledf,rebalance)
ladderNAV <- function(assetRtn,ruledf,rebalance=NULL){
  assetRtn[,c('pos','rtn','nav_rebalance','nav')] <- 0

  if(is.null(rebalance)){
    for(i in 1:nrow(assetRtn)){
      if(i==1){
        assetRtn$pos[i] <- ruledf$pos[1]
      }else{
        postmp <- findInterval(assetRtn$nav_rebalance[i-1],ruledf$nav)
        assetRtn$pos[i] <- ruledf$pos[postmp]
      }
      assetRtn$rtn[i] <- assetRtn$stock[i]*assetRtn$pos[i]+assetRtn$bond[i]*(1-assetRtn$pos[i])
      assetRtn$nav_rebalance[i] <- prod(1+assetRtn$rtn[1:i])
      assetRtn$nav[i] <- prod(1+assetRtn$rtn[1:i])
    }
  }else{
    reDate <- seq.Date(min(assetRtn$date),max(assetRtn$date),by = rebalance)
    reDate <- QDataGet::trday.nearby(reDate,by=0)
    for( i in 1:nrow(assetRtn)){
      if(assetRtn$date[i] %in% reDate){
        assetRtn$pos[i] <- ruledf$pos[1]
        j <- i
      }else{
        postmp <- findInterval(assetRtn$nav_rebalance[i-1],ruledf$nav)
        assetRtn$pos[i] <- ruledf$pos[postmp]
      }
      assetRtn$rtn[i] <- assetRtn$stock[i]*assetRtn$pos[i]+assetRtn$bond[i]*(1-assetRtn$pos[i])
      assetRtn$nav_rebalance[i] <- prod(1+assetRtn$rtn[j:i])
      assetRtn$nav[i] <- prod(1+assetRtn$rtn[1:i])
    }
  }

  if(is.null(rebalance)){
    assetRtn <- assetRtn[,c("date","stock","bond","pos","rtn","nav")]
  }
  return(assetRtn)
}


#' Organize and standardize the TSF object.
#'
#' @param tsf A data frame containg the following columns: date, stocks, factorscore.
#' @param reorder The vector indicating the index of date/stock/factorscore.
#' @return A TSF object with standard format.
#' @export
tidytsf <- function(tsf, reorder = NULL){
  if(!is.null(reorder)) {tsf = tsf[,reorder]}
  colnames(tsf) <- c("date","stockID","factorscore")
  if(is.integer(tsf$date))  tsf$date <- intdate2r(tsf$date)
  tsf$date <- as.Date(tsf$date)
  if(substr(tsf$stockID[1],1,2) != "EQ") tsf$stockID <- paste('EQ',substr(x = tsf$stockID,start = 1,stop = 6),sep = "")
  return(tsf)
}

#' Fill in the NA.
#'
#' @param vec A vector.
#' @param method The method to fill in the NA.
#' @return A vector without NA values.
#' @export
fillna <- function(vec, method = "mean"){
  match.arg(method, c("mean","median","zero"))
  if( method == "mean"){
    vec[is.na(vec)] = mean(vec, na.rm = TRUE)
  }else if( method == "median"){
    vec[is.na(vec)] = median(vec, na.rm = TRUE)
  }else if( method == "zero"){
    vec[is.na(vec)] = 0
  }
  return(vec)
}

#' Easy way to add up two TSF objects.
#'
#' @param tsf1,tsf2 The TSF objects.
#' @param wgt The vector of wights which must sum up to 1.
#' @return A TSF object.
#' @export
merge2tsf <- function(tsf1, tsf2, wgt = c(0.5,0.5)){
  stopifnot(sum(wgt)==1)
  tsf1 <- tidytsf(tsf1)
  tsf2 <- tidytsf(tsf2)
  tsf_c <- merge(tsf1,tsf2,by=c("date","stockID"),all=TRUE)
  tsf_c$factorscore.y[is.na(tsf_c$factorscore.y)] = tsf_c$factorscore.x[is.na(tsf_c$factorscore.y)]
  tsf_c$factorscore.x[is.na(tsf_c$factorscore.x)] = tsf_c$factorscore.y[is.na(tsf_c$factorscore.x)]
  tsf_c$factorscore <- tsf_c$factorscore.x*wgt[1]+tsf_c$factorscore.y*wgt[2]
  tsf <- subset(tsf_c, select=c("date","stockID","factorscore"))
  tsf <- tidytsf(tsf)
  return(tsf)
}

#' Transform daily TSF to montly TSF.
#'
#' @param ts The TS object.
#' @param db The data frame containing the daily TSF data.
#' @param window An interger indicating the time window to summarize. If NA, the time window will be the time interval between the rebalance dates.
#' @return A TSF object.
#' @export
getmonthFac <- function(ts, db, window = NA){
  QUtility::check.colnames(ts, c("date","stockID"))
  if(is.na(window)){
    v <- unique(ts$date)
    v0 <- QDataGet::trday.nearby(v[1], by = 20)
    v <- c(v0, v)
    ind <- findInterval(db$date, v, left.open = TRUE)
    db2 <- cbind(db, ind)
    stocklist <- unique(ts$stockID)
    db2 <- subset(db2, ind != 0)
    db2 <- subset(db2, stockID %in% stocklist)
    db2 <- dplyr::group_by(.data = db2, stockID, ind)
    db2 <- dplyr::summarise(db2, newfac = sum(factorscore))
    ind <- findInterval(TS$date, v, left.open = TRUE)
    ts2 <- cbind(ts, ind)
    re <- merge(ts2, db2, by = c("stockID","ind"), all.x = TRUE)
    re2 <- data.frame("date" = re$date, "stockID" = as.character(re$stockID), "factorscore" = re$newfac)
    return(re2)
  }else{
    stocklist <- unique(ts$stockID)
    db <- subset(db, stockID %in% stocklist)
    v1 <- unique(ts$date)
    v2 <- QDataGet::trday.nearby(v1, by = window)
    ind1 <- findInterval(db$date, v1, left.open = TRUE)
    ind2 <- findInterval(db$date, v2, left.open = FALSE)
    db2 <- cbind(db, ind1, ind2)
    db2 <- dplyr::group_by(db2, stockID, ind1)
    db2 <- dplyr::filter(db2, ind2 >= ind1 + 1)
    db2 <- dplyr::summarise(db2, newfac = sum(factorscore))
    ind1 <- findInterval(ts$date, v1, left.open = TRUE)
    ts2 <- cbind(ts, ind1)
    re <- merge(ts2,db2, by = c("stockID","ind1"), all.x = TRUE)
    re2 <- data.frame("date" = re$date, "stockID" = as.character(re$stockID), "factorscore" = re$newfac)
    return(re2)
  }
}

#' Get simple sector
#'
#' @param ts The object containing ts.
#' @return Return the object with additional column secID.
#' @details simple sector only contains 6 categories, ES1:Big Cycle, ES2:Finance and Real estate, ES3:TMT, ES4:Consumpstions, ES5:Manufactoring, ES6:Others.
#' @export
gf.ezsec <- function(ts){
  ts.tmp <- subset(ts, select = c("date","stockID"))
  ts.tmp <- RFactorModel::gf.sector(ts.tmp, sectorAttr = defaultSectorAttr())
  seclist <- list()
  # BigCycle
  seclist[[1]]<- c("ES33110000","ES33210000","ES33220000","ES33230000","ES33240000")
  #FinRealEstate
  seclist[[2]]<- c("ES33480000","ES33490000","ES33430000")
  #TMT
  seclist[[3]]<- c("ES33710000","ES33720000","ES33730000","ES33270000")
  #Comsump
  seclist[[4]]<- c("ES33280000","ES33330000","ES33340000","ES33350000","ES33460000","ES33370000","ES33450000")
  #Manufac
  seclist[[5]]<- c("ES33360000","ES33630000","ES33640000","ES33610000","ES33620000","ES33650000")
  #Others
  seclist[[6]]<- c("ES33420000","ES33410000","ES33510000")
  for(ii in 1:length(seclist)){
    V2 <- paste("ES",ii,sep = "")
    seclist[[ii]] <- as.data.frame(seclist[[ii]])
    seclist[[ii]] <- cbind(seclist[[ii]], V2)
    names(seclist[[ii]]) <- c("sector","secID")
  }
  secdf <- data.table::rbindlist(seclist)
  re <- merge(ts.tmp, secdf, by = c("sector"))
  re2 <- subset(re, select = c("date","stockID","secID"))
  re3 <- merge(ts,re2,by=c("date","stockID"))
  return(re3)
}

#' Wrap up tsRemoteCallFunc with additional StockID column
#'
#' @param funchar A character string indicating which function to use in tsRemoteCallFunc.
#' @param funpar A list for function parameters.
#' @param syspar A list for system parameters, eg. StockID.
#' @return A complicated list without unlist.
#' @export
wraptsfun <- function(funchar, funpar = NULL, syspar = NULL){
  # funpar <- list(begT = rdate2ts(as.Date("2014-01-01")),
  #                endT = rdate2ts(as.Date("2014-01-01")))
  # syspar <- list(StockID = StockID)
  tmp <- tsRemoteCallFunc(funchar = funchar, pars = funpar, syspars = syspar)
  tmp <- plyr::llply(.data = tmp, function(i) within(i, StockID <- StockID))
  return(tmp)
}


#' Expand ETS object into TS object with index
#'
#' @param ETS A event TS object which includes the event date and the corresponding stock.
#' @param win1 Integer. The time window of days before the event date.
#' @param win2 Integer. The time window of days after the event date.
#' @return A TS object with index.
EE_ExpandETS_1row <- function(ETS, win1 = 20, win2 = 60) {
  QUtility::check.colnames(ETS, c('date', 'stockID'))
  if(nrow(ETS) != 1L) {stop("this function can only be used on ETS of 1 observation.")}
  ETS$date <- QDataGet::trday.nearest(ETS$date, dir = -1)
  begT = QDataGet::trday.nearby(ETS$date, by = win1)
  endT = QDataGet::trday.nearby(ETS$date, by = -win2)
  res <- QDataGet::trday.get(begT = begT, endT = endT)
  ID <- rep(ETS$stockID, length(res))
  index <- c((-win1):(-1), 0, 1:win2)
  finalres <- data.frame('No' = index,'date' = res, 'stockID' = ID)
  return(finalres)
}

#' Plug in ETS and return a TS object with Err and index.
#'
#' @param ETS A event TS object which includes the event date and the corresponding stock.
#' @param db The name string of local database err sheet which containing the columns of "date", "stockID", "err".
#' @param win1 Integer. The time window of days before the event date.
#' @param win2 Integer. The time window of days after the event date.
#' @return A TS object with Err and index.
#' @export
#' @examples
#' date <- as.Date(c("2014-07-01","2014-07-08"))
#' stockID <- c("EQ000001","EQ000002")
#' ETS <- data.frame(date, stockID)
#' TSErr <- EE_GetTSErr(ETS)
#' EE_Plot(TSErr)
EE_GetTSErr <- function(ETS, db = "EE_CroxSecReg", win1 = 20, win2 = 60) {
  QUtility::check.colnames(ETS, c('date','stockID'))
  temp <- plyr::adply(.data = ETS, .margins = 1, .fun = function(x) EE_ExpandETS_1row(x, win1 = win1, win2 = win2))
  TargetTS <- subset(temp, select = c('No', 'date', 'stockID'))
  # write into lcdb, read back with left join with err
  TargetTS$date <- QUtility::rdate2int(TargetTS$date)
  con <- QDataGet::db.local()
  DBI::dbWriteTable(conn = con, value = TargetTS, name = 'mazi_tmp', overwrite = TRUE, append = FALSE, row.names = FALSE)
  qr <- paste (
    "select a.*, b.err
    from mazi_tmp a
    left join ", db, " b
    on a.stockID = b.stockID
    and a.date = b.date"
  )
  finalres <- DBI::dbGetQuery(con, qr)
  DBI::dbDisconnect(conn = con)
  # double check
  QUtility::check.colnames(finalres,c("No","date","stockID","err"))
  finalres$date <- QUtility::intdate2r(finalres$date)
  return(finalres)
}

#' Plug in TSErr object and return the summary plot
#'
#' @param TSErr The TSErr object which must containing No and err columns.
#' @return Two plots.
#' @export
#' @examples
#' date <- as.Date(c("2014-07-01","2014-07-08"))
#' stockID <- c("EQ000001","EQ000002")
#' ETS <- data.frame(date, stockID)
#' TSErr <- EE_GetTSErr(ETS)
#' EE_Plot(TSErr)
EE_Plot <- function(TSErr){
  TSErr$err <- fillna(TSErr$err, method = "zero")
  tmpdat <- plyr::ddply(.data = TSErr, .variables = "No", plyr::summarise, mean = mean(err))
  colnames(tmpdat) <- c("No","err")
  tmpvec <- cumprod(tmpdat$err+1)
  TSErr1 <- data.frame('No'=tmpdat$No, 'err' = tmpvec)
  p1 <-  ggplot2::ggplot()+
    ggplot2::geom_path(data = TSErr1, ggplot2::aes(x = No, y=err), size = 1) +
    ggplot2::geom_vline(xintercept = 0, color = 'red', linetype = 2)+
    ggplot2::ylab("Accumulated Abnormal Return")+ggplot2::xlab("Date Series")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
  TSErr2 <- tmpdat
  p2 <- ggplot2::ggplot()+
    ggplot2::geom_bar(data = TSErr2, ggplot2::aes(x = No, y=err), stat = 'identity')+
    ggplot2::geom_vline(xintercept = 0, color = 'red', linetype = 2)+
    ggplot2::ylab("Daily Abnormal Return")+ggplot2::xlab("Date Series")
  re <- QUtility::multiplot(plotlist = list(p1,p2), ncol=1)
  return(re)
}




#' Get ETS object from JY database
#'
#' @param stock.column The column name string of the stockID in the data.
#' @param stock.decode The column name string that is used to decode the stockID.
#' @param date.column The column name string of the date in the data.
#' @param SheetName The sheet name string of the data.
#' @param key.column The column name string of the key variable in the data.
#' @param key.decode The decoding number that is used to interpret the key.column.
#' @return A dataframe with date, stockID and var(key column).
#' @export
#' @examples
#' # Fetching the ETS of investors' activities.
#' ETS <- EE_getETSfromJY(stock.column = "InnerCode", stock.decode = "InnerCode",
#'                        date.column = "InfoPublDate", SheetName = "LC_InvestorRa", key.column = "SerialNb")
#'
EE_getETSfromJY <- function(stock.column = "InnerCode", stock.decode = "InnerCode",
                            date.column = "InfoPublDate",
                            SheetName, key.column, key.decode = NULL){
  if(is.null(key.decode)){
    qr <- paste(
      "select  convert(varchar(8),target.",date.column,",112) date,
      'EQ'+s.SecuCode stockID,
      target.",key.column," var
      from JYDB.dbo.",SheetName," target,
      JYDB.dbo.SecuMain s
      where target.",stock.column," = s.",stock.decode,"
      and s.SecuCategory in (1,2)"
    )
  }else{
    qr <- paste(
      "select  convert(varchar(8),target.",date.column,",112) date,
      'EQ'+s.SecuCode stockID,
      decode.MS var
      from JYDB.dbo.",SheetName," target,
      JYDB.dbo.SecuMain s,
      JYDB.dbo.CT_SystemConst decode
      where target.",stock.column," = s.",stock.decode,"
      and s.SecuCategory in (1,2)
      and target.",key.column," = decode.DM
      and decode.LB = ",key.decode
    )
  }
  temp <- RODBC::sqlQuery(QDataGet::db.jy(), qr, errors = FALSE)
  if(is.integer(temp)){
    if(temp == -1L){
      temp <- RODBC::sqlQuery(db.jy(), qr)
      warning("Getquery step failed.")
      return(temp)
    }
  }
  temp$date <- QUtility::intdate2r(temp$date)
  # double check
  QUtility::check.colnames(temp, c("date","stockID","var"))
  return(temp)
}



#' Split and return the plots of each year
#'
#' @param TSErr The TSErr object.
#' @param everyyear Logical value. Whether to return the plot of details in each year.
#' @param breakwindow Logical value. Whether to keep the event window complete. default is false.
#' @return A list containing plots and data.
#' @export
EE_splityear <- function(TSErr, everyyear = FALSE, breakwindow = FALSE){

  if(breakwindow == FALSE){
    coreETS <- subset(TSErr, No == 0, select = c("date", "stockID"))
    tmpyy <- lubridate::year(coreETS$date)
    yy <- rep(tmpyy, each = nrow(TSErr)/nrow(coreETS))
    yy <- as.factor(yy)
  }else{
    yy <- lubridate::year(TSErr$date)
    yy <- as.factor(yy)
  }
  TSErrlist <- split(TSErr, yy)
  tmplist <- list()
  for(i in 1:length(TSErrlist)){
    tmp <- TSErrlist[[i]]
    tmp$err <- fillna(tmp$err, method = "zero")
    tmpdat <- plyr::ddply(.data = tmp, .variables = "No", plyr::summarise, mean = mean(err))
    colnames(tmpdat) <- c("No","err")
    tmpvec <- cumprod(tmpdat$err+1)
    tmplist[[i]] <- data.frame('No'=tmpdat$No, 'err' = tmpvec)
    tmplist[[i]]$year <- levels(yy)[i]
  }
  TSErr1 <- data.table::rbindlist(tmplist)
  fig <- ggplot2::ggplot() +
    ggplot2::geom_path(data = TSErr1, ggplot2::aes(x = No, y=err, colour = year), size = 1) +
    ggplot2::geom_vline(xintercept = 0, color = 'red', linetype = 2)+
    ggplot2::ylab("Accumulated Abnormal Return") + ggplot2::xlab("Date Series")
  reslist <- list(fig)
  if(everyyear == TRUE){
    for( i in 1:length(TSErrlist)){
      tmp.fig <- EE_Plot(TSErrlist[[i]]) + ggplot2::ggtitle()
      reslist[[i+1]] <- tmp.fig
    }
    names(reslist) <- c("all years",levels(yy))
  }else{
    names(reslist) <- c("all years")
  }
  reslist$data <- TSErr1
  return(reslist)
}

