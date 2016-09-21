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

#' Transform daily data to montly data.
#'
#' @param df The data frame containing the daily data.
#' @param date The column of date.
#' @param value The column of factorscore.
#' @param std Logical values, whether normalize the factorscore.
#' @param tradingday Logical values, whether apply trday.nearest function.
#' @return A TSF object.
#' @export
daily2monthly <- function(df, date, value, std = TRUE, tradingday = TRUE){
  colnames(df)[colnames(df) == as.character(substitute(date))] = "date"
  colnames(df)[colnames(df) == as.character(substitute(value))] = "score.tmp"
  cols <- setdiff(colnames(df), "score.tmp")
  df$date <- QUtility::cut.Date2(as.Date(df$date), breaks = "month")
  df <- plyr::ddply(.data = df, .variables = cols, (summarise),
              factorscore = sum(score.tmp))
  if(tradingday == TRUE) {df$date <- QDataGet::trday.nearest(as.Date(df$date))}
  if(std == TRUE) {df <- RFactorModel:::factor.std(df, factorStd = "norm")}
  return(df)
}
