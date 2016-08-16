# Ladder Position Strategy

#' Function for calculate NAV for Ladder Position Strategy
#'
#' @param rtn A xts of daily return(value not in percentage).
#' @param pos A vector indicating the position values for each stage.
#' @param cutvalue A vector indicating the maximum NAV for each stage.
#'
#' @return Returning a data frame, containing return, postion, NAV_1, NAV_2 for each trading day.
#'
#' The NAV_1 represents the net asset value in the 2-years period, which means the NAV_1 is set to 1.00 at the beginning of every 2 years.
#'
#' The NAV_2 represents the net asset value since the first date.
#'
Ladder_NAV <- function(rtn,
                       pos = c(0.15,0.25,0.35,0.5),
                       cutvalue = c(1.05,1.10,1.20)){
  if(!is.xts(rtn)) stop("The rtn input is not xts class.")
  if(length(pos) != length(cutvalue)+1 ) stop("It's not a valid pair of pos and cutvalue.")

  newpos <- function(NV) {
    tmp.res <- NULL
    lll <- length(cutvalue)
    for(i in lll:1){
      if(NV < cutvalue[i]){tmp.res <- pos[i]}
    }
    if(is.null(tmp.res)){tmp.res <- pos[lll+1]}
    return(tmp.res)
  }

  flag <- vector("numeric",length(rtn))
  yyy <- year( index(rtn) )
  if(length(flag) != length(yyy)){stop(" flag error. ")}
  base <- yyy[1]
  flag = (yyy-base)%/%2+1

  len <- length(rtn)
  NAV_1 <- vector("numeric", len)
  NAV_2 <- vector("numeric", len)
  posvec <- vector("numeric", len)
  posvec[1] <- pos[1]
  NAV_1[1] <- (rtn[1]*posvec[1] + 1)*1
  NAV_2[1] <- (rtn[1]*posvec[1] + 1)*1
  tmp.flag = 1

  for( i in 2:len ){
    if(flag[i] > tmp.flag){
      posvec[i] = pos[1]
      aa <- rtn[i] * posvec[i] + 1
      NAV_1[i] = aa * 1
      NAV_2[i] = aa * NAV_2[i-1]
      tmp.flag = flag[i]
    }else{
      posvec[i] = newpos(NAV_1[i-1])
      aa <- rtn[i] * posvec[i] + 1
      NAV_1[i] =  aa *  NAV_1[i-1]
      NAV_2[i] =  aa * NAV_2[i-1]
    }
  }
  res <- cbind(rtn,posvec,NAV_1,NAV_2)
  return(res)
}

