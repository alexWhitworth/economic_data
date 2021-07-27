
#----------------------------------------------------------
# FUNCTIONS
#----------------------------------------------------------
quarterly_yoy <- function(x) {
  nx <- length(x)
  min_idx <- which(!is.na(x))[1]
  num_na <- min_idx + 14
  yoy <- qtr <- vector("numeric", length= nx)
  for (i in 3:nx) {
    qtr[i] <- yoy[i] <- NA
    qtr[i] <- sum(x[(i-2):i])
    if (i > num_na) {
      yoy[i] <- qtr[i] / qtr[i - 12]
    }
  }
  yoy <- ifelse(yoy == 0, NA, yoy)
  return(yoy)
}

## ADD ggplot functions to reduce typing -- allow for time ranges
# @param dt A \code{data.table} or \code{data.frame} object
# @param date_min A \code{date}. To left-truncate the data. Defaults to \code{NULL}
# @param date_seq A \code{vector} of \code{Date}s giving the x-axis breaks
# @param use_index Logical. Do you want the quarterly_yoy series (FALSE, default) or the index?
# @param y_range A \code{vector} of length 2 giving the y axis range
ggplot_series <- function(dt, date_min= NULL, date_seq, date_max= max(dt[,date]),
                          use_index= FALSE, y_range= c(0.85, 1.2),
                          facet= c("wrap", "grid")) {
  require(ggplot2)
  
  if (!is.null(date_min) & class(date_min) == "Date") {
    dt <- dt[date >= date_min,]
  }
  
  if (!use_index) {
    p <- ggplot(dt, aes(x= date, y= quarterly_yoy, group= series)) + geom_line() +
      scale_y_continuous(breaks= round(seq(y_range[1], y_range[2], length.out= 10), 3),
                         limits= c(y_range[1], y_range[2]))
  } else {
    p <- ggplot(dt, aes(x= date, y= index, group= series)) + geom_line() + 
      scale_y_continuous(breaks= round(seq(y_range[1], y_range[2], length.out= 10), 3),
                         limits= c(y_range[1], y_range[2]))
  }
  if (facet == "wrap") {
    p <- p + facet_wrap(~ series)
  } else if (facet == "grid") {
    p <- p + facet_grid(series ~ .)
  } else {
    error("facet must be one of ('wrap', 'grid')")
  }
  
  p <- p + geom_hline(yintercept= 1, colour= "red", alpha= 0.5) +
    annotate("rect", xmin= as.Date("2007-12-01", format= "%Y-%m-%d"), 
             xmax= as.Date("2009-06-30", format= "%Y-%m-%d"), ymin= -Inf, ymax= Inf, alpha=0.1) +
    annotate("rect", xmin= as.Date("2020-03-01", format= "%Y-%m-%d"), 
             xmax= as.Date('2020-04-30'), ymin= -Inf, ymax= Inf, alpha=0.1) +
    scale_x_date(breaks= date_seq, limits= c(date_seq[1], date_seq[length(date_seq)])) +
    theme(legend.position = "bottom",
          axis.title= element_text(face="bold", size= 12),
          axis.text.x= element_text(size= 10, angle= 90),
          axis.text.y= element_text(size= 10),
          plot.title= element_text(face= "bold", size= 14))
  
  return(p)
}

