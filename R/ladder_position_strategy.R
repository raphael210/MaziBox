#' Function for calculate NAV for Ladder Position Strategy
#'
#' @param stockrtn A xts of daily return(value not in percentage).
#' @param pos A vector indicating the position values for each stage.
#' @param cutvalue A vector indicating the maximum NAV for each stage.
#' @return Returning a data frame, containing return, postion, NAV_1, NAV_2 for each trading day. NAV_1 represents the net asset value with rebalance every 2 years. NAV_2 represents the net asset value wihout rebalance.
#' @export
ladderNAV <- function(stockrtn, bondrtn,
                       pos = c(0.15,0.25,0.35,0.5),
                       cutvalue = c(1.05,1.10,1.20)){
  # input check
  if(!(xts::is.xts(stockrtn))) stop("The stockrtn input is not xts class.")
  if(length(pos) != length(cutvalue)+1 ) stop("It's not a valid pair of pos and cutvalue.")
  if(!missing(bondrtn)){
    if(!(xts::is.xts(bondrtn))) stop("The bondrtn input is not xts class.")
    if(!all(index(stockrtn) == index(bondrtn))) stop("The stockrtn and bondrtn does not match.")
  }
  # sub-func
  newpos <- function(NV) {
    tmp.res <- NULL
    lll <- length(cutvalue)
    for(i in lll:1){
      if(NV < cutvalue[i]){tmp.res <- pos[i]}
    }
    if(is.null(tmp.res)){tmp.res <- pos[lll+1]}
    return(tmp.res)
  }

  # construct flag/rebalance cycle index
  flag <- vector("numeric",length(stockrtn))
  yyy <- lubridate::year( index(stockrtn) )
  if(length(flag) != length(yyy)){stop(" flag error. ")}
  base <- yyy[1]
  flag = (yyy-base)%/%2+1

  # initiate variables
  len <- length(stockrtn)
  NAV_1 <- vector("numeric", len)
  NAV_2 <- vector("numeric", len)
  posvec <- vector("numeric", len)
  posvec[1] <- pos[1]
  NAV_1[1] <- (stockrtn[1]*posvec[1] + 1)*1
  NAV_2[1] <- (stockrtn[1]*posvec[1] + 1)*1
  tmp.flag = 1

  # calc NAV
  if(missing(bondrtn)){
    for( i in 2:len ){
      if(flag[i] > tmp.flag){ # rebalance to 1
        posvec[i] = pos[1]
        aa <- stockrtn[i] * posvec[i] + 1
        NAV_1[i] = aa * 1
        NAV_2[i] = aa * NAV_2[i-1]
        tmp.flag = flag[i]
      }else{
        posvec[i] = newpos(NAV_1[i-1])
        aa <- stockrtn[i] * posvec[i] + 1
        NAV_1[i] =  aa * NAV_1[i-1]
        NAV_2[i] =  aa * NAV_2[i-1]
      }
    }
  }else{
    for( i in 2:len ){
      if(flag[i] > tmp.flag){ # rebalance to 1
        posvec[i] = pos[1]
        aa <- stockrtn[i] * posvec[i] + bondrtn[i] * (1-posvec[i]) + 1
        NAV_1[i] = aa * 1
        NAV_2[i] = aa * NAV_2[i-1]
        tmp.flag = flag[i]
      }else{
        posvec[i] = newpos(NAV_1[i-1])
        aa <- stockrtn[i] * posvec[i] + bondrtn[i] * (1-posvec[i]) + 1
        NAV_1[i] =  aa * NAV_1[i-1]
        NAV_2[i] =  aa * NAV_2[i-1]
      }
    }
  }

  # organize and output
  if(missing(bondrtn)){
    res <- cbind(stockrtn,posvec,NAV_1,NAV_2)
  }else{
    res <- cbind(stockrtn,bondrtn,posvec,NAV_1,NAV_2)
  }
  return(res)
}

# Get bond daily return from WindR
# return bond daily return from WindR
# getBondrtn <- function(startdate = "2009-01-01", enddate = "2016-06-30", code = "037.CS"){
#   library(WindR)
#   w.start()
#   w_wsd_data<-WindR::w.wsd(code,"pct_chg",startdate,enddate)
#   tmp <- w_wsd_data$Data
#   bondrtn <- xts::as.xts(x=tmp$PCT_CHG, order.by = tmp$DATETIME)
#   names(bondrtn) = "bond"
#   bondrtn  =  bondrtn / 100
#   return(bondrtn)
# }
