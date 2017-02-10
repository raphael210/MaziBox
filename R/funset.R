# ----- Others -----

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
#' @author han.qian
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

# ----- MultiFac Model -----

#' Fill in the NA.
#'
#' @param vec A vector.
#' @param method The method to fill the NAs, could be median, mean, zero.
#' @return A vector.
#' @export
fillna <- function(vec, method = "mean", trim = NA){
  match.arg(method, c("mean","median","zero"))
  if( method == "mean"){
    if(is.na(trim)){
      vec[is.na(vec)] = mean(vec, na.rm = TRUE)
    }else{
      vec[is.na(vec)] = mean(vec, na.rm = TRUE, trim = trim)
    }
  }else if( method == "median"){
    vec[is.na(vec)] = median(vec, na.rm = TRUE)
  }else if( method == "zero"){
    vec[is.na(vec)] = 0
  }
  return(vec)
}

#' Transform daily TSF to montly TSF.
#'
#' @param ts The TS object.
#' @param db The database containing the daily TSF data.
#' @param window An interger indicating the time window to summarize. If NA, the time window will be the time interval between the rebalance dates.
#' @return A TSF object.
#' @export
getmonthFac <- function(ts, db, window = NA){
  QUtility::check.colnames(ts, c("date","stockID"))
  if(is.na(window)){
    v <- unique(ts$date)
    stocklist <- unique(ts$stockID)
    v0 <- QDataGet::trday.nearby(v[1], by = -20)
    v <- c(v0, v)
    ind <- findInterval(db$date, v, left.open = TRUE)
    db2 <- cbind(db, ind)
    db2 <- subset(db2, ind != 0)
    db2 <- subset(db2, stockID %in% stocklist)
    db2 <- dplyr::group_by(.data = db2, stockID, ind)
    db2 <- dplyr::summarise(db2, newfac = sum(factorscore))
    ind <- findInterval(ts$date, v, left.open = TRUE)
    ts2 <- cbind(ts, ind)
    re <- merge(ts2, db2, by = c("stockID","ind"), all.x = TRUE)
    re2 <- data.frame("date" = re$date, "stockID" = as.character(re$stockID), "factorscore" = re$newfac)
    return(re2)
  }else{
    stocklist <- unique(ts$stockID)
    db <- subset(db, stockID %in% stocklist)
    v1 <- unique(ts$date)
    v2 <- QDataGet::trday.nearby(v1, by = -window)
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

#' Get tradingday by passing in vector inputs.
#'
#' @param begTvec begT vector.
#' @param endTvec endT vector.
#' @return A vector that row-bind all the trading days between begT and endT.
#' @export
trday.get.vec <- function(begTvec, endTvec){
  if(length(begTvec)!=length(endTvec)) stop("The lengths of begTvec and endTvec do not match.")
  begT.min <- min(begTvec)
  endT.max <- max(endTvec)
  T.df <- data.frame("begT" = begTvec, "endT" = endTvec)
  # get the market trading days
  begTT <- max(begT.min,as.Date("1990-12-19"))
  begTT <- QUtility::rdate2int(begTT)
  endTT <- QUtility::rdate2int(endT.max)
  qr <- paste("select TradingDate from QT_TradingDay where SecuMarket=83 and IfTradingDay=1 and TradingDate between ",begTT,"and",endTT)
  trday <- QDataGet::queryAndClose.dbi(QDataGet::db.local(),qr)
  trday$TradingDate <- QUtility::intdate2r(trday$TradingDate)
  T.df <- dplyr::rowwise(T.df)
  temp <- dplyr::do(T.df, subset(trday, TradingDate >= .$begT  &  TradingDate <= .$endT))
  colnames(temp) <- "date"
  return(temp$date)
}

#' Count trading days by passing vector inputs.
#'
#' @param begTvec begT vector.
#' @param endTvec endT vector.
#' @return A vector which each row represents the lag of days between begT and endT.
#' @export
trday.count.vec <- function(begTvec, endTvec){
  if(length(begTvec)!=length(endTvec)) stop("The lengths of begTvec and endTvec do not match.")
  mmin <- min(min(begTvec), min(endTvec))
  mmax <- max(max(begTvec), max(endTvec))
  T.df <- data.frame("begT" = begTvec, "endT" = endTvec)
  T.df2 <- T.df
  ind <- T.df$begT > T.df$endT
  T.df2$begT[ind] = T.df$endT[ind]
  T.df2$endT[ind] = T.df$begT[ind]

  # get the market trading days
  begTT <- max(mmin,as.Date("1990-12-19"))
  begTT <- QUtility::rdate2int(begTT)
  endTT <- QUtility::rdate2int(mmax)
  qr <- paste("select TradingDate from QT_TradingDay where SecuMarket=83 and IfTradingDay=1 and TradingDate between ",begTT,"and",endTT)
  trday <- QDataGet::queryAndClose.dbi(QDataGet::db.local(),qr)
  trday$TradingDate <- QUtility::intdate2r(trday$TradingDate)

  #
  T.df2 <- dplyr::rowwise(T.df2)
  temp <- dplyr::do(T.df2, as.data.frame(nrow(subset(trday, TradingDate >= .$begT  &  TradingDate <= .$endT))))
  colnames(temp) <- "diff"
  temp[ind,] <- temp[ind,]*(-1)
  return(temp$diff)
}

#' plug in stockID and return indexID
#'
#' @param stocklist vector.
#' @return indexlist, vector.
#' @export
stock2index <- function(stocklist){
  # step 1
  key.df <- rbind(buildkey(kkey = "Standard"),
                  buildkey(kkey = "Industry", decode = 1804),
                  buildkey(kkey = "IfPerformed"),
                  buildkey(kkey = "FirstIndustryCode"),
                  buildkey(kkey = "FirstIndustryName"))
  tmpdat <- EE_getETSfromJY("LC_ExgIndustry",key.df, stock.column = "CompanyCode", stock.decode = "CompanyCode")
  tmpdat <- subset(tmpdat, Standard == 24 & IfPerformed == 1)
  tmpdat <- renameCol(tmpdat, "FirstIndustryCode", "IndustryCode")
  # step 2
  tmpdat2 <- queryAndClose.odbc(db.jy(),
                                "select A.*, B.SecuCode
                                from JYDB.dbo.LC_CorrIndexIndustry A,
                                JYDB.dbo.SecuMain B
                                where A.IndexCode = B.InnerCode")
  tmpdat2 <- subset(tmpdat2, IndustryStandard == 24 , select = c("IndexCode","IndustryCode", "IndexState","SecuCode"))
  tmpdat2 <- subset(tmpdat2, substr(SecuCode,1,3) == "801")
  # merge
  re <- merge(tmpdat, tmpdat2, by = c("IndustryCode"), all.x = TRUE)
  re$SecuCode <- paste0("EI", re$SecuCode)
  # merge
  rawdat <- data.frame("stockID" = stocklist)
  re2 <- merge(rawdat, re, by = "stockID" , all.x = T)
  return(re2$SecuCode)
}

# ----- Event Effect Research -----

#' build key for key data frame
#'
#' @param kkey The column name.
#' @param decode The decoding number if the values have to be interpreted by the CT_SystemConst sheet.
#' @param isSE NA if the variable is not in the sub-sheet. Otherwise, look up the SE sub-sheet and fill in the TypeCode of the variable.
#' @return A single row in the data frame format.
#' @details Rbind the keys to one single data frame and apply it in the the EE_getETSfromJY function.
#' @export
buildkey <- function(kkey,decode,isSE){
  re <- data.frame(kkey = kkey)
  re$decode <- ifelse(missing(decode),NA,decode)
  re$isSE <- ifelse(missing(isSE),NA,isSE)
  return(re)
}

#' Get ETS object from JY database
#'
#' @param SheetName The sheet name string of the data.
#' @param key.df The key data frame.
#' @param extra.condition The extra qr string could be add to the SQL.
#' @param stock.column The column name which indicating the stockID in the sheet
#' @param stock.decode The column name in decode sheet which is used to decode the stockID.
#' @param date.column The column name string of the date in the data.
#' @return A dataframe with date, stockID and vars.
#' @export
#' @examples
#' # Fetching the ETS of investors' activities.
#' key.df <- rbind(buildkey("SerialNb"))
#' tmpdat <- EE_getETSfromJY(SheetName = "LC_InvestorRa", key.df)
#' # Fetching the ETS of shares transfering.
#' key.df <- rbind(buildkey("TranShareType",1040),
#'                 buildkey("TranMode",1202),
#'                 buildkey("IfSuspended"),
#'                 buildkey("TransfererEcoNature",1096,1),
#'                 buildkey("ReceiverEcoNature",1096,2))
#' tmpdat <- EE_getETSfromJY(SheetName = "LC_ShareTransfer", key.df)
EE_getETSfromJY <- function(SheetName, key.df,
                            extra.condition = NULL,
                            stock.column = "InnerCode",
                            stock.decode = "InnerCode",
                            date.column = "InfoPublDate"){
  nkey <- nrow(key.df)
  varqr <- " "
  sheetqr <- " "
  decodeqr <- " "
  namevec <- c()
  for( i in 1:nkey){
    if(is.na(key.df$decode[i])){
      varqr <- paste0(varqr,","," target.",key.df$kkey[i]," ","var",i)
      namevec <- c(namevec, as.character(key.df$kkey[i]))
    }else{
      if(is.na(key.df$isSE[i])){
        varqr <- paste0(varqr,","," target.",key.df$kkey[i]," ","var",i)
        varqr <- paste0(varqr,","," decode",i,".MS"," ","var_",i)
        sheetqr <- paste0(sheetqr,","," JYDB.dbo.CT_SystemConst decode",i)
        decodeqr <- paste0(decodeqr," ","and target.",key.df$kkey[i]," = decode",i,".DM")
        decodeqr <- paste0(decodeqr," ","and decode",i,".LB=",key.df$decode[i])
      }else{
        varqr <- paste0(varqr,","," targetse",i,".Code var",i)
        varqr <- paste0(varqr,","," decode",i,".MS"," ","var_",i)
        sheetqr <- paste0(sheetqr,","," JYDB.dbo.CT_SystemConst decode",i)
        sheetqr <- paste0(sheetqr,","," JYDB.dbo.",SheetName,"_SE targetse",i)
        decodeqr <- paste0(decodeqr," ","and target.ID=targetse",i,".ID")
        decodeqr <- paste0(decodeqr," ","and targetse",i,".TypeCode=",key.df$isSE[i])
        decodeqr <- paste0(decodeqr," ","and decode",i,".LB=",key.df$decode[i])
        decodeqr <- paste0(decodeqr," ","and targetse",i,".Code=decode",i,".DM")
      }
      namevec <- c(namevec, as.character(key.df$kkey[i]), paste0(key.df$kkey[i],"_decode"))
    }
  }
  qr <- paste0(
    "select convert(varchar(8),target.",date.column,",112) date,
    'EQ'+s.SecuCode stockID",varqr,"
    from JYDB.dbo.",SheetName," target,
    JYDB.dbo.SecuMain s", sheetqr,"
    where target.",stock.column,"=s.",stock.decode,"
    and s.SecuCategory in (1,2)",decodeqr
  )
  if(!is.null(extra.condition)){
    qr <- paste(qr, extra.condition, sep = " and ")
  }
  temp <- QDataGet::queryAndClose.odbc(QDataGet::db.jy(), qr)
  if(!is.null(nrow(temp))){
    temp$date <- QUtility::intdate2r(temp$date)
    colnames(temp) <- c("date","stockID", namevec)
  }
  return(temp)
}

#' Expand ETS object into TS object with index. Multi-row version.
#'
#' @param ets A event TS object which includes the event date and the corresponding stock.
#' @param win1 Integer. The time window of days before the event date.
#' @param win2 Integer. The time window of days after the event date.
#' @return A TS object with index.
#' @export
EE_ExpandETS <- function(ets, win1, win2){
  QUtility::check.colnames(ets, c("date","stockID"))
  len <- win1+1+win2
  stockID.col <- rep(ets$stockID, each = len)
  date.col <- QDataGet::trday.nearest(ets$date, dir = 1)
  begT.col <- QDataGet::trday.nearby(date.col, by = -win1)
  endT.col <- QDataGet::trday.nearby(date.col, by = win2)
  # T.df <- data.frame("begT" = begT.col, "endT" = endT.col)
  date.col <- trday.get.vec(begT.col, endT.col)
  if(win1 > 0){
    b <- c((-win1:-1),0:win2)
    No <- rep(b, nrow(ets))
  }else{
    b <- 0:win2
    No <- rep(b, nrow(ets))
  }
  res <- data.frame("No" = No, "date" = date.col, "stockID" = stockID.col)
  return(res)
}

#' Plug in ETS and return a TS object with Err and index.
#'
#' @param ETS A event TS object which includes the event date and the corresponding stock.
#' @param db The name string of local database err sheet which containing the columns of "date", "stockID", "err".
#' @param win1 Integer. The time window of days before the event date.
#' @param win2 Integer. The time window of days after the event date.
#' @param cleansing Logical values. If true, the result will be fill NA and winsorized before out put.
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
  TargetTS <- EE_ExpandETS(ETS, win1 = win1, win2 = win2)
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
  # if(cleansing){
  #   finalres$err <- fillna(finalres$err, method = "zero")
  #   finalres <- dplyr::group_by(finalres, No)
  #   finalres <- dplyr::mutate(finalres, err = robustHD::winsorize(err))
  # }
  return(finalres)
}

#' Plug in TSErr object and return the summary plot
#'
#' @param TSErr The TSErr object which must containing No and err columns.
#' @return Plot
#' @export
#' @examples
#' date <- as.Date(c("2014-07-01","2014-07-08"))
#' stockID <- c("EQ000001","EQ000002")
#' ETS <- data.frame(date, stockID)
#' TSErr <- EE_GetTSErr(ETS)
#' EE_Plot(TSErr)
EE_Plot <- function(TSErr, bmk = NULL){
  if(!is.null(bmk)){
    bmk$err <- fillna(bmk$err, method = "zero")
  }
  TSErr$err <- fillna(TSErr$err, method = "zero")
  TSErr0 <- dplyr::group_by(TSErr, No)
  TSErr0 <- dplyr::mutate(TSErr0, err = robustHD:::winsorize(err, const = 3))
  p0 <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = TSErr0, ggplot2::aes(x= No, group = No, y =err))+
    ggplot2::geom_vline(xintercept = -0.5, color = 'red', linetype = 2)+
    ggplot2::ylab("Abnormal Return")+ggplot2::xlab("Date Series")
  print(p0)
  cat("Press any key to continue")
  line <- readline()
  if(!is.null(bmk)){
    tmpbmk <- dplyr::group_by(bmk, No)
    tmpbmk <- dplyr::summarise(tmpbmk, mean = mean(err), std = sqrt(var(err)))
    colnames(tmpbmk) <- c("No","err","std")
    tmpdat <- dplyr::group_by(TSErr, No)
    tmpdat <- dplyr::summarise(tmpdat, mean = mean(err), std = sqrt(var(err)))
    colnames(tmpdat) <- c("No","err","std")
    tmpdat$err <- tmpdat$err - tmpbmk$err
    tmpvec <- cumprod(tmpdat$err+1)
  }else{
    tmpdat <- dplyr::group_by(TSErr, No)
    tmpdat <- dplyr::summarise(tmpdat, mean = mean(err), std = sqrt(var(err)))
    colnames(tmpdat) <- c("No","err","std")
    tmpvec <- cumprod(tmpdat$err+1)
  }
  TSErr1 <- data.frame('No'=tmpdat$No, 'err' = tmpvec, 'std' = tmpdat$std)
  p1 <-  ggplot2::ggplot()+
    ggplot2::geom_ribbon(data = TSErr1, ggplot2::aes(x = No, ymin=err-std, ymax=err+std), fill="grey70")+
    ggplot2::geom_path(data = TSErr1, ggplot2::aes(x = No, y=err), size = 1) +
    ggplot2::geom_vline(xintercept = -1, color = 'red', linetype = 2)+
    ggplot2::ylab("Accumulated Abnormal Return")+ggplot2::xlab("Date Series")+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
  TSErr2 <- tmpdat
  p2 <- ggplot2::ggplot()+
    ggplot2::geom_bar(data = TSErr2, ggplot2::aes(x = No, y=err), stat = 'identity')+
    ggplot2::geom_vline(xintercept = -0.5, color = 'red', linetype = 2)+
    ggplot2::ylab("Daily Abnormal Return")+ggplot2::xlab("Date Series")
  re <- QUtility::multiplot(plotlist = list(p1,p2), ncol=1)
  return(re)
}

#' Plug in TSErr object and return the summary table
#'
#' @param TSErr The TSErr object
#' @return Table.
#' @export
EE_table <- function(TSErr){
  TSErr$err <- fillna(TSErr$err, method = "zero")
  tmpdat <- dplyr::group_by(TSErr, No)
  tmpdat <- dplyr::summarise(tmpdat, mean = mean(err), std = sqrt(var(err)))
  # tmpdat <- plyr::ddply(.data = TSErr, .variables = "No", plyr::summarise, mean = mean(err))
  colnames(tmpdat) <- c("No","err","std")
  return(tmpdat)
}

#' Split and return the plots of each year
#'
#' @param TSErr The TSErr object.
#' @param everyyear Logical value. Whether to return the plot of details in each year.
#' @param breakwindow Logical value. Whether to keep the event window complete or forcely cut the object by year. Default is false.
#' @return A list containing plots and data.
#' @export
EE_splityear <- function(TSErr, everyyear = FALSE, breakwindow = FALSE, bmk = NULL){
   if(!is.null(bmk)){
     bmk <- dplyr::arrange(bmk, date, stockID)
    if(breakwindow == FALSE){
      coreETS_0 <- subset(bmk, No == 0, select = c("date", "stockID"))
      tmpyy_0 <- lubridate::year(coreETS_0$date)
      yy_0 <- rep(tmpyy_0, each = nrow(bmk)/nrow(coreETS_0))
      yy_0 <- as.factor(yy_0)
      bmk$yy <- yy_0
    }else{
      yy_0 <- lubridate::year(bmk$date)
      yy_0 <- as.factor(yy_0)
      bmk$yy <- yy_0
    }
   }
  TSErr <- dplyr::arrange(TSErr, date, stockID)
  if(breakwindow == FALSE){
    coreETS <- subset(TSErr, No == 0, select = c("date", "stockID"))
    tmpyy <- lubridate::year(coreETS$date)
    yy <- rep(tmpyy, each = nrow(TSErr)/nrow(coreETS))
    yy <- as.factor(yy)
    TSErr$yy <- yy
  }else{
    yy <- lubridate::year(TSErr$date)
    yy <- as.factor(yy)
    TSErr$yy <- yy
  }
  yylist <- unique(TSErr$yy)
  tmplist <- list()
  if(!is.null(bmk)){
    if( !all(unique(yy) %in% unique(yy_0)) ){
      stop("bmk time sequence is not complete")
    }
  }
  for(i in 1:length(yylist)){
    tmp <- subset(TSErr, yy == yylist[i])
    tmp$err <- fillna(tmp$err, method = "zero")
    tmpdat <- plyr::ddply(.data = tmp, .variables = "No", plyr::summarise, mean = mean(err))
    colnames(tmpdat) <- c("No","err")
    if(!is.null(bmk)){
      tmpbmk <- subset(bmk, yy == yylist[i])
      tmpbmk$err <- fillna(tmpbmk$err, method = "zero")
      tmpdatbmk <- plyr::ddply(.data = tmpbmk, .variables = "No", plyr::summarise, mean = mean(err))
      colnames(tmpdatbmk) <- c("No","err")
      tmpdat$err <- tmpdat$err - tmpdatbmk$err
    }
    tmpvec <- cumprod(tmpdat$err+1)
    tmplist[[i]] <- data.frame('No'=tmpdat$No, 'err' = tmpvec)
    tmplist[[i]]$year <- levels(yy)[i]
  }
  TSErr1 <- data.table::rbindlist(tmplist)
  fig <- ggplot2::ggplot() +
    ggplot2::geom_path(data = TSErr1, ggplot2::aes(x = No, y=err, colour = year), size = 1) +
    ggplot2::geom_vline(xintercept = -1, color = 'red', linetype = 2)+
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

#' plug in ets and return frequency of certain period.
#'
#' @param ets A ets object.
#' @param win1 The days window before the event to calculate the frequency.
#' @return etsobj
#' @export
etstick <- function(ets, win1 = 20){
  begT <- min(ets$date)
  endT <- max(ets$date)
  date <- QDataGet::trday.get(begT, endT)
  fillindate <- data.frame(date, stockID = "None")
  tmp <- rbind(ets, fillindate)
  tmp <- dplyr::arrange(tmp, date, stockID)
  tmp <- reshape2::dcast(tmp, formula = date~stockID)
  extra <- tmp[1:(win1-1),]
  extra[,-1] <- 0
  extra[,1] <- tmp[1,1]-(win1-1):1
  tmp <- rbind(extra,tmp)
  tmp <- xts::xts(tmp[,-1], order.by = tmp[,1])
  re <- zoo::rollsum(tmp, win1, align = "right")
  date <- zoo::index(re)
  re2 <- as.data.frame(re)
  re2 <- cbind(date, re2)
  re3 <- reshape2::melt(re2, id = 'date')
  colnames(re3) <- c("date","stockID","ticks")
  re4 <- merge(ets, re3, by=c("date","stockID"))
  re4 <- dplyr::arrange(re4, date, stockID)
  re4 <- re4[!duplicated(re4),]
  return(re4)
}

#' plug in ets and return accumulated value of certain varialbe.
#'
#' @param etsobj An etsobj.
#' @param win1 The days window before the event to calculate the accumulated value.
#' @return etsobj
#' @export
etstack <- function(etsobj, win1 = 20){
  begT <- min(etsobj$date)
  endT <- max(etsobj$date)
  date <- QDataGet::trday.get(begT,endT)
  colnames(etsobj) <- c("date","stockID","var")
  fillindate <- data.frame(date, stockID = "None", var = 0)
  tmp <- rbind(etsobj, fillindate)
  tmp <- dplyr::arrange(tmp, date, stockID)
  tmp <- reshape2::dcast(tmp, formula = date~stockID, fun.aggregate = sum, value.var = "var")
  extra <- tmp[1:(win1-1),]
  extra[,-1] <- 0
  extra[,1] <- tmp[1,1]-(win1-1):1
  tmp <- rbind(extra,tmp)
  tmp[is.na(tmp)] <- 0
  tmp <- xts::xts(tmp[,-1], order.by = tmp[,1])
  re <- zoo::rollsum(tmp, win1, align = "right")
  date <- zoo::index(re)
  re2 <- as.data.frame(re)
  re2 <- cbind(date, re2)
  re3 <- reshape2::melt(re2, id = 'date')
  colnames(re3) <- c("date","stockID","tacks")
  re4 <- merge(etsobj[,c("date","stockID")], re3, by=c("date","stockID"))
  re4 <- dplyr::arrange(re4, date, stockID)
  re4 <- re4[!duplicated(re4),]
  return(re4)
}

#' adjusted subset function for Chinese string.
#'
#' @param tmpdat The data frame.
#' @param colchar The column names.
#' @param subsetcode The code for the subset that is going to be filtered.
#' @return data frame subset.
subsetCol <- function(tmpdat, colchar, subsetcode){
  tmpdat <- QUtility::renameCol(tmpdat, colchar, "char")
  tmpdat <- merge(tmpdat, EE_CT, by = "char", all.x = TRUE)
  tmpdat <- subset(tmpdat, code == subsetcode)
  tmpdat <- tmpdat[,setdiff(colnames(tmpdat), c("char","code"))]
  return(tmpdat)
}

# ----- ETS factor function -----

#' plug in ts object and return with ets object.
#'
#' @param tsobj A ts object.
#' @param EventSet a vector to specify the events. If null, all the events in DefaultEventSet will be applied to use.
#' @return A data frame with date, stockID, event and the lag of days.
#' @export
getETS <- function(tsobj, EventSet = NULL, win = 20){

  ts <- tsobj[,c("date","stockID")]
  datelist <- unique(ts$date)
  # load all
  con <- QDataGet::db.local()
  EE_pool <- DBI::dbReadTable(con,'EE_pool')
  DBI::dbDisconnect(con)
  EE_pool$date <- QUtility::intdate2r(EE_pool$date)
  if(!is.null(EventSet)){
    EE_pool <- subset(EE_pool, event %in% EventSet)
  }
  finalre <- list()

  for(i in 1:length(datelist)){
    tmpts <- subset(ts, date == datelist[i])
    startdate <- QDataGet::trday.nearby(datelist[i], by = -win)
    enddate <- QDataGet::trday.nearby(datelist[i], by = win)
    # normal case
    tmppool1 <- subset(EE_pool, date >= startdate & date <= datelist[i] & event != "ets.unfroz")
    # special case : unfroz
    tmppool2 <- subset(EE_pool, date >= datelist[i]-1 & date <= enddate & event == "ets.unfroz")
    tmppool <- rbind(tmppool1,tmppool2)
    if(nrow(tmppool) == 0) next
    tmptmp <- rep(datelist[i], nrow(tmppool))
    tmppool$diff <- trday.count.vec(begTvec =  tmppool$date, endTvec = tmptmp)
    tmppool$date <- datelist[i]
    rownames(tmppool) <- NULL
    tmppool <- dplyr::arrange(tmppool, date, stockID, diff)
    finalre[[i]] <- tmppool
  }
  finalre <- data.table::rbindlist(finalre)
  return(finalre)
}

#' plug in ts object and return with ets score.
#'
#' @param tsobj A tsobj.
#' @param EventSet a vector to specify the events. If null, all the events in DefaultEventSet will be applied to use.
#' @return ts object with event score.
#' @export
getETSscore <- function(tsobj, EventSet = NULL, rollwin = 20){

  # build ee_score_sum df
  con <- QDataGet::db.local()
  EE_score <- DBI::dbReadTable(con,'EE_score')
  DBI::dbDisconnect(con)
  if(!is.null(EventSet)){
    EE_score <- subset(EE_score, event %in% EventSet)
  }
  eventlist <- unique(EE_score$event)
  EE_score_sum <- list()
  for( i in 1:length(eventlist)){
    subdat <- subset(EE_score, event == eventlist[i], select = c("No","err","event"))
    errsum <- zoo::rollsum(subdat$err, k = rollwin, align = "left")
    subdat <- subdat[1:length(errsum),]
    subdat$err <- errsum
    EE_score_sum[[i]] <- subdat
    gc()
  }
  EE_score_sum <- data.table::rbindlist(EE_score_sum)
  ####

  ts <- tsobj[,c("date","stockID")]
  ets <- getETS(ts, EventSet = EventSet, win = rollwin)
  ets <- QUtility::renameCol(ets, "diff", "No")

  re <- merge(ets, EE_score_sum, by = c("event","No"))
  re <- dplyr::arrange(re, date, stockID)
  re <- re[,c("date","stockID","err")]
  re <- QUtility::renameCol(re, "err","eventscore")
  re2 <- merge.x(tsobj, re)
  re2$eventscore <- fillna(re2$eventscore, "zero")
  return(re2)
}

# ----- ETS -----

#' get ETS of stocks unfrozing
#'
#' @return ETS object.
#' @export
ets.unfroz <- function(){
  df_jy <- EE_getETSfromJY(date.column = "StartDateForFloating", SheetName = "LC_SharesFloatingSchedule", key.df = data.frame("kkey"=c("Proportion1","SourceType"),"decode"=c(NA,NA),"isSE"=c(NA,NA)))
  df_jy <- subset(df_jy, Proportion1>5 & SourceType %in% c(24,25))
  ETS <- subset(df_jy, select = c("date","stockID"))
  return(ETS)
}

#' get ETS of stocks with low forecasting
#'
#' @return ETS object.
#' @export
ets.low_F_NP <- function(){
  begT <- as.Date("2005-01-31")
  endT <- Sys.Date()-1
  RebDates <- RFactorModel::getRebDates(begT,endT)
  ts <- RFactorModel::getTS(RebDates,"EI000985")
  tsf <- QFactorGet::gf.F_NP_chg(ts,span='w4')
  tmp <- na.omit(tsf)
  tmp <- tmp[tmp$factorscore<(-1),]
  ts2 <- subset(tmp,select=c("date","stockID"))
  return(ts2)
}

#' get ETS of leader selling stocks.
#'
#' @return ETS object.
#' @export
ets.leadersell <- function(){
  con <- QDataGet::db.local()
  qr <- "select * from EE_LeaderStockAlter"
  tmpdat <- DBI::dbGetQuery(con,qr)
  DBI::dbDisconnect(con)
  tmpdat <- subsetCol(tmpdat, "shareholder_type" ,3)
  tmpdat <- subsetCol(tmpdat, "direction", 4)
  tmpdat2 <- subset(tmpdat, select = c("announcement_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  tmpdat2 <- dplyr::arrange(tmpdat2, date, stockID)
  return(tmpdat2)
}

#' get ETS of leader buying stocks.
#'
#' @return ETS object.
#' @export
ets.leaderbuy <- function(){
  con <- QDataGet::db.local()
  qr <- "select * from EE_LeaderStockAlter"
  tmpdat <- DBI::dbGetQuery(con,qr)
  DBI::dbDisconnect(con)
  tmpdat <- subsetCol(tmpdat, "shareholder_type" ,3)
  tmpdat <- subsetCol(tmpdat, "direction", 5)
  tmpdat2 <- subset(tmpdat, select = c("announcement_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  tmpdat2 <- dplyr::arrange(tmpdat2, date, stockID)
  return(tmpdat2)
}

#' get ETS of leader selling stocks a lot within a few times.
#'
#' @return ETS object.
#' @export
ets.leadersell_largesell <- function(){
  con <- QDataGet::db.local()
  qr <- "select * from EE_LeaderStockAlter"
  tmpdat <- dbGetQuery(con,qr)
  dbDisconnect(con)
  tmpdat <- subsetCol(tmpdat, "shareholder_type" ,3)
  tmpdat <- subsetCol(tmpdat, "direction", 4)

  tmpdat2 <- subset(tmpdat, select = c("announcement_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  tmpdat2 <- dplyr::arrange(tmpdat2, date, stockID)
  tmpdat3 <- etstick(tmpdat2, 20)

  tmpdat2_v <- subset(tmpdat, select = c("announcement_date","stockID","value_change"))
  colnames(tmpdat2_v) <- c("date","stockID","var")
  tmpdat2_v$date <- QUtility::intdate2r(tmpdat2_v$date)
  tmpdat2_v <- dplyr::arrange(tmpdat2_v, date, stockID)
  tmpdat3_v <- etstack(tmpdat2_v, 20)

  re1 <- subset(tmpdat3, ticks <= 10)
  re2 <- subset(tmpdat3_v, tacks >= 1000)
  re <- merge(re1, re2, by = c("date","stockID"))

  re <- subset(re, select = c("date","stockID"))
  return(re)
}

#' get ETS of leader buying stocks a lot within a few times.
#'
#' @return ETS object.
#' @export
ets.leaderbuy_largebuy <- function(){

  con <- QDataGet::db.local()
  tmpdat <- DBI::dbReadTable(con,'EE_LeaderStockAlter')
  DBI::dbDisconnect(con)
  tmpdat <- subsetCol(tmpdat, "shareholder_type", 3)
  tmpdat <- subsetCol(tmpdat, "direction", 5)

  tmpdat2 <- subset(tmpdat, select = c("announcement_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  tmpdat2 <- dplyr::arrange(tmpdat2, date, stockID)
  tmpdat3 <- etstick(tmpdat2, 20)

  tmpdat2_v <- subset(tmpdat, select = c("announcement_date","stockID","change_proportion_floating"))
  colnames(tmpdat2_v) <- c("date","stockID","var")
  tmpdat2_v$date <- QUtility::intdate2r(tmpdat2_v$date)
  tmpdat2_v <- dplyr::arrange(tmpdat2_v, date, stockID)
  tmpdat3_v <- etstack(tmpdat2_v, 20)

  re1 <- subset(tmpdat3, ticks <= 3)
  re2 <- subset(tmpdat3_v, tacks > 0.125)
  re <- merge(re1, re2, by = c("date","stockID"))
  re <- subset(re, select = c("date","stockID"))
  return(re)
}

#' get ETS of employee plan
#'
#' @return ETS object
#' @export
ets.employeeplan <- function(){
  con <- QDataGet::db.local()
  qr <- "select * from EE_EmployeePlan"
  tmpdat <- DBI::dbGetQuery(con,qr)
  # as.data.frame(table(tmpdat$fund_source))
  mark <- summary(tmpdat$shares_ratio)[[3]]
  tmpdat2 <- subset(tmpdat, shares_ratio >= mark ,select = c("preplan_date","stockID"))
  colnames(tmpdat2) <- c("date","stockID")
  tmpdat2$date <- QUtility::intdate2r(tmpdat2$date)
  return(tmpdat2)
}

#' get ETS of forecast report
#'
#' @return ETS object
#' @export
ets.forecast <- function(season = c("all",'1','2','3','4'), pool=c("002","300","zhuban","all")){
  con <- db.local()
  season <- match.arg(season)
  pool <- match.arg(pool)
  qr <- paste("select stockID, enddate, date, ForcastType, EGrowthRateFloor, EProfitFloor, ActualDate
              from LC_ForecastAndReport")
  res <- dbGetQuery(con, qr)
  res <- subset(res, ForcastType == 4)
  if(season == '1'){
    res <- subset(res, substr(enddate, 5,8) == "0331")
  }else if( season == '2'){
    res <- subset(res, substr(enddate, 5,8) == "0630")
  }else if( season == '3'){
    res <- subset(res, substr(enddate, 5,8) == "0930")
  }else if( season == '4'){
    res <- subset(res, substr(enddate, 5,8) == "1231")
  }
  res <- na.omit(res)
  if( pool == "002"){
    res <- subset(res, substr(stockID, 1, 5) == "EQ002")
  }else if( pool == "300"){
    res <- subset(res, substr(stockID, 1, 5) == "EQ300")
  }else if( pool == "zhuban"){
    res <- subset(res, substr(stockID, 1, 5) != "EQ002")
    res <- subset(res, substr(stockID, 1, 5) != "EQ300")
  }
  datelist <- sort(unique(res$enddate))
  finalres <- list()
  s1 <- 0
  s2 <- 0
  for( i in 1:length(datelist)){
    tmpres <- subset(res, enddate == datelist[i])
    if(nrow(tmpres) < 20) next
    tmpres_ <- subset(tmpres, EGrowthRateFloor >= s1 & EProfitFloor >= s2)
    finalres[[i]] <- tmpres_[,c("date","stockID")]
    colnames(finalres[[i]]) <- c("date","stockID")
    s1 <- quantile(tmpres$EGrowthRateFloor, 0.25)
    s2 <- quantile(tmpres$EProfitFloor, 0.25)
  }
  finalres2 <- data.table::rbindlist(finalres)
  finalres2$date <- QUtility::intdate2r(finalres2$date)
  finalres2 <- na.omit(finalres2)
  return(finalres2)
}

#' ets.belowExpectation
#'
#' @return ETS object.
#' @export
ets.belowExpectation <- function(){

  # RPTDATE SEQ
  sq1 <- seq(2000, 2017)
  sq2 <- c('0331','0630','0930','1231')
  rptsq <- c()
  for(i in sq1){
    rptsq <- c(rptsq, paste0(i,sq2))
  }
  rptsq <- as.integer(rptsq)

  # DATA SET 0, FORECAST
  con <- db.local()
  qr <- paste("select * from LC_ForecastAndReport")
  tmpdat <- dbGetQuery(con,qr)
  dbDisconnect(con)
  ind <- !is.na(tmpdat$EGrowthRateFloor) & !is.na(tmpdat$EGrowthRateCeiling)
  tmpdat0 <- tmpdat[ind,]

  # DATA SET 1, LAST FORECAST
  tmpdat1 <- subset(tmpdat0, select=c("stockID","enddate","date","EGrowthRateFloor","EGrowthRateCeiling"))
  colnames(tmpdat1) <- c("stockID","enddate","date","L.EGrowthRateFloor","L.EGrowthRateCeiling")
  tmpdat1 <- dplyr::arrange(tmpdat1, stockID, enddate, date)
  tmpdat1 <- tmpdat1[!duplicated(tmpdat1,fromLast = T),]
  tmpdat1 <- dplyr::select(tmpdat1, -date)
  ind <- match(tmpdat1$enddate, rptsq)
  ind <- ind + 1
  tmpdat1$enddate <- rptsq[ind]

  # merge
  finalre <- merge(tmpdat0, tmpdat1, by = c("stockID","enddate"), all.x = T)
  finalre <- dplyr::arrange(finalre, stockID, enddate)

  # compute flags
  # flag1
  finalre$flag1 <- 0
  ind1 <- finalre$EGrowthRateCeiling < finalre$L.EGrowthRateCeiling
  ind1[is.na(ind1)] <- FALSE
  ind2 <- finalre$EGrowthRateFloor < finalre$L.EGrowthRateFloor
  ind2[is.na(ind2)] <- FALSE
  finalre$flag1[ind1] <- finalre$flag1[ind1] + 1
  finalre$flag1[ind2] <- finalre$flag1[ind2] + 1

  finalrere <- subset(finalre, flag1 >= 2 & ForcastType == 4, select = c("date","stockID"))
  finalrere <- na.omit(finalrere)
  finalrere$date <- intdate2r(finalrere$date)
  return(finalrere)
}

# ----- TS Screening -----

#' filter stocks to remove negative event.
#'
#' @param tsobj An object contains ts.
#' @return tsobj.
#' @export
rms.all <- function(tsobj){
  tsobj <- rms.unfroz(tsobj)
  tsobj <- rms.low_F_NP(tsobj)
  return(tsobj)
}

#' filter stocks that are going to be unfrozen.
#'
#' @param tsobj An object contains ts.
#' @return tsobj.
#' @export
rms.unfroz <- function(tsobj){
  ts <- tsobj[,c("date","stockID")]
  endT <- max(ts$date)
  ETS <- ets.unfroz()
  ETS <- subset(ETS, date <= endT + 30)
  ETS <- EE_ExpandETS(ETS, win1 = 25, win2 = 1)
  TS2 <- subset(ETS, select = c("date","stockID"))
  TS2$stockID <- as.character(TS2$stockID)
  re <- dplyr::setdiff(ts,TS2)
  re2 <- merge(re, tsobj, by = c("date","stockID"), all.x = TRUE)
  return(re2)
}

#' fitler stocks that have low NP forcast
#'
#' @param tsobj An object contains ts.
#' @return tsobj.
#' @export
rms.low_F_NP <- function(tsobj){
  ts <- tsobj[,c("date","stockID")]
  tsf <- QFactorGet::gf.F_NP_chg(ts,span='w4')
  tmp <- na.omit(tsf)
  tmp <- tmp[tmp$factorscore<(-1),]
  ts2 <- subset(tmp,select=c("date","stockID"))
  re <- dplyr::setdiff(ts,ts2)
  re2 <- merge(re, tsobj, by=c("date","stockID"), all.x = TRUE)
  return(re2)
}

# ----- LCDB.build & update -----

#' lcdb.build.EE_CroxSecReg
#'
#' @export
lcdb.build.EE_CroxSecReg <- function(begT,endT,factorLists){
  if(missing(begT)){
    begT <- as.Date("2005-01-04")
  }
  if(missing(endT)){
    endT <- Sys.Date()-1
  }
  if(missing(factorLists)){
    factorLists = buildFactorLists(
      buildFactorList(factorFun = "gf.ln_mkt_cap", factorStd = "norm", factorNA = "na"))
  }
  RebDates <- getRebDates(begT,endT,rebFreq = "day")
  monthind <- cut.Date2(RebDates,"month")
  monthlist <- unique(monthind)
  loopind <- data.frame(RebDates, monthind)
  for( ii in 1:(length(monthlist))){
    cat(rdate2int(as.Date(monthlist[ii])),"\n")
    loopind_ <- subset(loopind, monthind == monthlist[ii])
    RebDates_ <- loopind_$RebDates
    TS_ <- getTS(RebDates_, indexID = "EI000985")
    res_list <- suppressWarnings(reg.TS(TS = TS_, dure = lubridate::days(1), factorLists = factorLists,
                                        regType = "glm", glm_wgt = "sqrtFV"))
    finalre <- res_list$res
    finalre <- renameCol(finalre, "res", "err")
    finalre$date <- trday.nearby(finalre$date, by = 1)
    finalre$date <- rdate2int(finalre$date)
    con <- QDataGet::db.local()
    if(ii == 1){
      RSQLite::dbWriteTable(con,'EE_CroxSecReg',finalre,overwrite=T,append=F,row.names=F)
    }else{
      RSQLite::dbWriteTable(con,'EE_CroxSecReg',finalre,overwrite=F,append=T,row.names=F)
    }
    RSQLite::dbDisconnect(con)
    gc()
  }
  return("Done!")
}

#' lcdb.update.EE_CroxSecReg
#'
#' @export
lcdb.update.EE_CroxSecReg <- function(begT, endT, factorLists){

  con <- db.local()
  qr <- paste0("select max(date) from EE_CroxSecReg")
  begT_lcdb <- dbGetQuery(con, qr)[[1]]
  dbDisconnect(con)
  begT_lcdb <- intdate2r(begT_lcdb)

  if(missing(begT)){
    begT <- begT_lcdb
  }else{
    begT <- trday.nearest(begT)
    if(begT <= begT_lcdb){
      begT_ <- rdate2int(begT)
      con <- db.local()
      qr <- paste0("delete from EE_CroxSecReg
                   where date >= ", begT_)
      RSQLite::dbSendQuery(con,qr)
      RSQLite::dbDisconnect(con)
      begT <- trday.nearby(begT,by = -1)
    }else if(begT > begT_lcdb){
      begT <- begT_lcdb
    }
  }

  if(missing(endT)){
    endT <- Sys.Date() - 1
    endT <- trday.nearest(endT)
  }

  if(begT >= endT){
    return("It's already up-to-date.")
  }else{
    if(missing(factorLists)){
      factorLists = buildFactorLists(
        buildFactorList(factorFun = "gf.ln_mkt_cap", factorStd = "norm", factorNA = "na"))
    }
    begT <- trday.nearby(begT, by = 1)
    RebDates <- getRebDates(begT,endT,rebFreq = "day")
    monthind <- cut.Date2(RebDates,"month")
    monthlist <- unique(monthind)
    loopind <- data.frame(RebDates, monthind)
    for( ii in 1:(length(monthlist))){
      cat(rdate2int(as.Date(monthlist[ii])),"\n")
      loopind_ <- subset(loopind, monthind == monthlist[ii])
      RebDates_ <- loopind_$RebDates
      TS_ <- getTS(RebDates_, indexID = "EI000985")
      res_list <- suppressWarnings(reg.TS(TS = TS_, dure = lubridate::days(1), factorLists = factorLists,
                                          regType = "glm", glm_wgt = "sqrtFV"))
      finalre <- res_list$res
      finalre <- renameCol(finalre, "res", "err")
      finalre$date <- trday.nearby(finalre$date, by = 1)
      finalre$date <- rdate2int(finalre$date)
      con <- QDataGet::db.local()
      RSQLite::dbWriteTable(con,'EE_CroxSecReg',finalre,overwrite=F,append=T,row.names=F)
      RSQLite::dbDisconnect(con)
      gc()
    }
    return("Done!")
  }
}


#' lcdb.build.EE_LeaderStockAlter
#'
#' @examples
#' library(WindR)
#' lcdb.build.EE_LeaderStockAlter()
#' @export
lcdb.build.EE_LeaderStockAlter <- function(){
  WindR::w.start(showmenu = FALSE)
  res <- data.frame()
  s.date <- c()
  e.date <- c()
  for(ii in 1990:2015){
    s.date <- c(s.date, paste(ii,"-01-01", sep=""))
    e.date <- c(e.date, paste(ii,"-12-31", sep=""))
  }
  len <- length(s.date)
  for(ii in 1:len){
    w_wset_data <- WindR::w.wset('majorholderdealrecord',startdate=s.date[ii],enddate=e.date[ii],'sectorid=a001010100000000;type=announcedate')
    tmp <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(tmp)
    if(nrow(tmp) == 0) next
    tmp <- tmp[, setdiff(colnames(tmp),"CODE")]
    tmp <- QUtility::renameCol(tmp,"wind_code","stockID")
    tmp$stockID <- QDataGet::stockID2stockID(tmp$stockID, from = "wind", to = "local")
    tmp$announcement_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$announcement_date, asdate = T))
    tmp$change_start_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_start_date, asdate = T))
    tmp$change_end_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_end_date, asdate = T))
    res = rbind(res,tmp)
  }
  res <- dplyr::arrange(res, announcement_date, stockID)
  con <- QDataGet::db.local()
  RSQLite::dbWriteTable(con,'EE_LeaderStockAlter',res,overwrite=T,append=F,row.names=F)
  RSQLite::dbDisconnect(con)
  return('Done!')
}

#' lcdb.update.EE_LeaderStockAlter
#'
#' @examples
#' library(WindR)
#' lcdb.update.EE_LeaderStockAlter()
#' @export
lcdb.update.EE_LeaderStockAlter <- function(){
  con <- QDataGet::db.local()
  re <- DBI::dbReadTable(con,'EE_LeaderStockAlter')
  begT <- QUtility::intdate2r(max(re$announcement_date))
  begT <- begT+1
  endT <- Sys.Date()
  oldyy <- lubridate::year(begT)
  newyy <- lubridate::year(endT)
  while(newyy > oldyy){
    WindR::w.start(showmenu = F)
    endT.tmp <- as.Date(paste(oldyy,"-12-31",sep=""))
    w_wset_data<- WindR::w.wset('majorholderdealrecord',startdate=begT,enddate=endT.tmp,'sectorid=a001010100000000;type=announcedate')
    tmp <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(tmp)
    if(nrow(tmp) != 0) {
      tmp <- tmp[, setdiff(colnames(tmp),"CODE")]
      tmp <- QUtility::renameCol(tmp,"wind_code","stockID")
      tmp$stockID <- QDataGet::stockID2stockID(tmp$stockID, from = "wind", to = "local")
      tmp$announcement_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$announcement_date, asdate = T))
      tmp$change_start_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_start_date, asdate = T))
      tmp$change_end_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_end_date, asdate = T))
      tmp <- dplyr::arrange(tmp, announcement_date, stockID)
      DBI::dbWriteTable(con, "EE_LeaderStockAlter", tmp, overwrite=F, append=T, row.names=F)
      DBI::dbDisconnect(con)
    }
    begT = as.Date(paste(oldyy+1,"-01-01",sep=""))
    oldyy <- lubridate::year(begT)
  }
  if(begT >= endT){
    return('Done!')
  }else{
    WindR::w.start(showmenu = F)
    w_wset_data<- WindR::w.wset('majorholderdealrecord',startdate=begT,enddate=endT,'sectorid=a001010100000000;type=announcedate')
    tmp <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(tmp)
    if(nrow(tmp) == 0) {
      return('Done!')
    }else{
      tmp <- tmp[, setdiff(colnames(tmp),"CODE")]
      tmp <- QUtility::renameCol(tmp,"wind_code","stockID")
      tmp$stockID <- QDataGet::stockID2stockID(tmp$stockID, from = "wind", to = "local")
      tmp$announcement_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$announcement_date, asdate = T))
      tmp$change_start_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_start_date, asdate = T))
      tmp$change_end_date <- QUtility::rdate2int(WindR::w.asDateTime(tmp$change_end_date, asdate = T))
      tmp <- dplyr::arrange(tmp, announcement_date, stockID)
      DBI::dbWriteTable(con, "EE_LeaderStockAlter", tmp, overwrite=F, append=T, row.names=F)
      DBI::dbDisconnect(con)
      return('Done!')
    }
  }
}

#' lcdb.build.EE_employeeplan
#'
#' @export
#' @examples
#' library(WindR)
#' lcdb.build.EE_employeeplan()
lcdb.build.EE_employeeplan <- function(){
  WindR::w.start(showmenu = FALSE)
  res <- data.frame()
  s.date <- as.Date("2014-01-01")
  e.date <- Sys.Date()
  w_wset_data<-w.wset('esop',startdate=s.date,enddate=e.date)
  if(w_wset_data$ErrorCode == 0){
    res <- w_wset_data$Data
    res <- res[,setdiff(colnames(res), "CODE")]
    res <- QUtility::renameCol(res, "wind_code", "stockID")
    res$stockID <- QDataGet::stockID2stockID(res$stockID, from = "wind", to = "local")
    res$preplan_date <- QUtility::rdate2int(WindR::w.asDateTime(res$preplan_date, asdate = T))
    res$fellow_smtganncedate <- QUtility::rdate2int(WindR::w.asDateTime(res$fellow_smtganncedate, asdate = T))
    res <- res[!is.na(res$stockID),]
    res <- dplyr::arrange(res, preplan_date, stockID)
    con <- QDataGet::db.local()
    RSQLite::dbWriteTable(con,'EE_EmployeePlan',res,overwrite=T,append=F,row.names=F)
    RSQLite::dbDisconnect(con)
    return("Done!")
  }else{
    return(w_wset_data)
  }
}

#' lcdb.update.EE_employeeplan
#'
#' @export
#' @examples
#' library(WindR)
#' lcdb.update.EE_employeeplan()
lcdb.update.EE_employeeplan <- function(){
  con <- QDataGet::db.local()
  re <- DBI::dbReadTable(con,'EE_EmployeePlan')
  begT <- QUtility::intdate2r(max(re$preplan_date))
  begT <- begT+1
  endT <- Sys.Date()
  if(begT >= endT){
    return("Done!")
  }else{
    WindR::w.start(showmenu = F)
    w_wset_data<-w.wset('esop',startdate=begT,enddate=endT)
    tmp <- w_wset_data$Data
    if(w_wset_data$ErrorCode != 0) return(tmp)
    if(nrow(tmp) == 0){
      return("Done!")
    }else{
      res <- tmp
      res <- res[,setdiff(colnames(res), "CODE")]
      res <- QUtility::renameCol(res, "wind_code", "stockID")
      res$stockID <- QDataGet::stockID2stockID(res$stockID, from = "wind", to = "local")
      res$preplan_date <- QUtility::rdate2int(WindR::w.asDateTime(res$preplan_date, asdate = T))
      res$fellow_smtganncedate <- QUtility::rdate2int(WindR::w.asDateTime(res$fellow_smtganncedate, asdate = T))
      res <- res[!is.na(res$stockID),]
      res <- dplyr::arrange(res, preplan_date, stockID)
      con <- QDataGet::db.local()
      RSQLite::dbWriteTable(con,'EE_EmployeePlan',res,overwrite=F,append=T,row.names=F)
      RSQLite::dbDisconnect(con)
      return("Done!")
    }
  }
}

#' lcdb.build.EE_score
#'
#' @export
lcdb.build.EE_score <- function(){
  re <- list()
  for(i in 1:length(DefaultEventSet)){
    ets <- eval(call(DefaultEventSet[i]))
    ets <- subset(ets, date <= Sys.Date())
    tserr <- EE_GetTSErr(ets, win1 = 60, win2 = 60)
    re[[i]] <- EE_table(tserr)
    re[[i]]$event <- DefaultEventSet[i]
  }
  finalre <- data.table::rbindlist(re)
  con <- QDataGet::db.local()
  RSQLite::dbWriteTable(con,'EE_score',finalre,overwrite=T,append=F,row.names=F)
  RSQLite::dbDisconnect(con)
  return("Done!")
}

#' lcdb.build.EE_pool
#'
#' @export
lcdb.build.EE_pool <- function(){
  re <- list()
  for(i in 1:length(DefaultEventSet)){
    re[[i]] <- eval(call(DefaultEventSet[i]))
    re[[i]]$event <- DefaultEventSet[i]
  }
  finalre <- data.table::rbindlist(re)
  finalre$date <- QUtility::rdate2int(finalre$date)
  finalre <- dplyr::arrange(finalre, date, stockID, event)
  con <- QDataGet::db.local()
  RSQLite::dbWriteTable(con,'EE_pool',finalre,overwrite=T,append=F,row.names=F)
  RSQLite::dbDisconnect(con)
  return("Done!")
}

#' lcdb.build.EE_ForecastAndReport
#'
#' @export
lcdb.build.EE_ForecastAndReport <- function(){
  stocklist <- getIndexComp("EI000985")
  funchar = "infoarray(128)"
  tmpfile <- stockID2stockID(stocklist,from="local",to="ts")
  tmpcsv <- tempfile(fileext=".csv")
  tmpcsv2 <- stringr::str_replace_all(tmpcsv,'\\\\',"\\\\\\\\")
  write.csv(tmpfile,tmpcsv,row.names=FALSE,quote=FALSE)
  qrstr <- paste0('oV:=BackUpSystemParameters();
                  rdo2 importfile(ftcsv(),"","',tmpcsv2,'",stockframe);
                  factorexp:=&"',funchar,'";
                  result:=array();
                  for i:=0 to length(stockframe)-1 do
                  begin
                  SetSysParam(pn_stock(),stockframe[i]["x"]);
                  factorvalue:=eval(factorexp);
                  result[i]:=factorvalue;
                  end;
                  RestoreSystemParameters(oV);
                  return result;
                  ')
  fct <- tsRemoteExecute(qrstr)
  for( i in 1:length(fct)){
    fct[[i]] <- plyr::ldply(fct[[i]], unlist)
    fct[[i]]$stockID <- stocklist[i]
  }
  fct <- data.table::rbindlist(fct)
  colnames(fct) <- c("enddate",
                     "FirstReservedDate","FirstChangeDate","SecondChangeDate",
                     "ThirdChangeDate","ActualDate",
                     "FirstReservePublDate","FirstChangePublDate","SecondChangePublDate",
                     "ThirdChangePublDate","ActualPublDate",
                     "stockID")

  qr <- paste("select t.*, 'EQ'+s.SecuCode stockID,
            convert(varchar(8),t.InfoPublDate,112) date,
            convert(varchar(8),t.EndDate,112) enddate
            from dbo.LC_PerformanceForecast t,
            dbo.SecuMain s
            where t.CompanyCode = s.CompanyCode
            and s.SecuCategory in (1,2)")
  fct2 <- queryAndClose.odbc(db.jy(), qr)
  fct2 <- dplyr::select(fct2, -ID, -InfoPublDate, -EndDate)

  res <- merge(fct2, fct, by = c("stockID","enddate"), all = TRUE)

  con <- QDataGet::db.local()
  RSQLite::dbWriteTable(con,'EE_ForecastAndReport',res,overwrite=T,append=F,row.names=F)
  RSQLite::dbDisconnect(con)
  return("Done!")
}

# ----- LCDB.build & update -----

#' rpt.unfroz_show
#'
#' @export
rpt.unfroz_show <- function(){
  ets0 <- ets.unfroz()
  colnames(ets0) <- c("unfroz_date", "stockID")
  ets0 <- subset(ets0, unfroz_date >= Sys.Date())

  TD <- Sys.Date()
  ets0$begT <- trday.nearby(ets0$unfroz_date, -10)
  ets0 <- subset(ets0, begT <= Sys.Date())
  ets0$date_end <- trday.nearby(TD,-1)
  temp_ <- getPeriodrtn(stockID = ets0$stockID, begT = ets0$begT, endT = ets0$date_end)
  temp_ <- renameCol(temp_, "periodrtn", "periodrtn_stock")
  temp_$indexID <- stock2index(temp_$stockID)
  temp_$periodrtn_index <- 0
  for( i in 1:nrow(temp_)){
    rtn <- getIndexQuote(stocks = temp_$indexID[i], begT = temp_$begT[i], endT = temp_$endT[i], variables = "pct_chg")
    temp_$periodrtn_index[i] <- sum(rtn$pct_chg)
  }
  re <- merge(ets0, temp_, by = c("stockID","begT"))
  re <- dplyr::select(re, -date_end, -endT)
  re <- renameCol(re, src = "periodrtn_stock", tgt = "acc_pct_chg_since_tracing")
  re <- renameCol(re, src = "begT", tgt = "tracing_start_date")
  re$acc_pct_chg_since_tracing <- fillna(re$acc_pct_chg_since_tracing, "zero")
  re$acc_pct_chg_since_tracing <- round(re$acc_pct_chg_since_tracing, 4)
  re <- dplyr::arrange(re, tracing_start_date)
  return(re)
}
