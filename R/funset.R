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
#' ruledf <- data.frame(nav = c(1,1.05,1.10,1.20),pos = c(0.15,0.25,0.35,0.5))
#' re <- ladderNAV(assetRtn,ruledf)
#' rebalance <- '2 years'
#' re <- ladderNAV(assetRtn,ruledf,rebalance)
ladderNAV <- function(assetRtn,ruledf,rebalance=NULL){

  assetRtn$pos <- c(0)
  assetRtn$rtn <- c(0)
  assetRtn$nav_rebalance <- c(0)
  assetRtn$nav <- c(0)

  assetRtn$pos[1] <- ruledf$pos[1]
  assetRtn$rtn[1] <- assetRtn$stock[1]*assetRtn$pos[1]+assetRtn$bond[1]*(1-assetRtn$pos[1])
  assetRtn$nav_rebalance[1] <- 1+assetRtn$rtn[1]
  assetRtn$nav[1] <- 1+assetRtn$rtn[1]

  if(is.null(rebalance)){
    for( i in 2:nrow(assetRtn)){
      postmp <- findInterval(assetRtn$nav_rebalance[i-1],ruledf$nav)
      if(postmp==0) postmp <- 1
      assetRtn$pos[i] <- ruledf$pos[postmp]
      assetRtn$rtn[i] <- assetRtn$stock[i]*assetRtn$pos[i]+assetRtn$bond[i]*(1-assetRtn$pos[i])
      assetRtn$nav_rebalance[i] <- (1+assetRtn$rtn[i])*assetRtn$nav_rebalance[i-1]
      assetRtn$nav[i] <- (1+assetRtn$rtn[i])*assetRtn$nav[i-1]
    }
  }else{
    reDate <-  RFactorModel::getRebDates(min(assetRtn$date),max(assetRtn$date),rebFreq = rebalance)
    reDate <- as.Date(setdiff(reDate,c(min(assetRtn$date),max(assetRtn$date))),origin = '1970-01-01')
    for( i in 2:nrow(assetRtn)){
      if(assetRtn$date[i] %in% reDate){
        assetRtn$pos[i] <- ruledf$pos[1]
        assetRtn$rtn[i] <- assetRtn$stock[i]*assetRtn$pos[i]+assetRtn$bond[i]*(1-assetRtn$pos[i])
        assetRtn$nav_rebalance[i] <- 1+assetRtn$rtn[i]
        assetRtn$nav[i] <- (1+assetRtn$rtn[i])*assetRtn$nav[i-1]
      }else{
        postmp <- findInterval(assetRtn$nav_rebalance[i-1],ruledf$nav)
        if(postmp==0) postmp <- 1
        assetRtn$pos[i] <- ruledf$pos[postmp]
        assetRtn$rtn[i] <- assetRtn$stock[i]*assetRtn$pos[i]+assetRtn$bond[i]*(1-assetRtn$pos[i])
        assetRtn$nav_rebalance[i] <- (1+assetRtn$rtn[i])*assetRtn$nav_rebalance[i-1]
        assetRtn$nav[i] <- (1+assetRtn$rtn[i])*assetRtn$nav[i-1]
      }
    }
  }

  if(is.null(rebalance)){
    assetRtn <- assetRtn[,c("date","stock","bond","pos","rtn","nav")]
  }
  return(assetRtn)
}

