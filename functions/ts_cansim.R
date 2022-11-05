
ts_cansim <- function(vector, start, end = NA){
  
  # load required packages
  require(cansim)
  require(lubridate)
  
  # get series
  if(is.na(end)){
    xx <- get_cansim_vector(vector , start_time = start)
  } else {
    xx <- get_cansim_vector(vector , start_time = start, end_time = end)
  } 
  
  # determine frequency
  time.diff <- as.Date(xx$REF_DATE[2]) - as.Date(xx$REF_DATE[1])
  
  x <- xx$VALUE
  n <- nrow(xx)
  
  st.date <- as.Date(xx$REF_DATE[1])
  end.date <- as.Date(xx$REF_DATE[n])
  
  # daily frequency
  if (1 <= time.diff & time.diff <= 4){
    n.freqency = 365.25
    x <- ts(xx$VALUE, start = c(year(st.date), month(st.date), day(st.date)), freq = n.freqency)
  }
  
  # weekly frequency
  if (time.diff == 7){
    n.freqency = 365.25/7
    x <- ts(xx$VALUE, start = c(year(st.date), month(st.date), day(st.date)), freq = n.freqency)
  }
  
  # monthly frequency 
  if (25 < time.diff & time.diff <= 31){
    n.freqency = 12
    x <- ts(xx$VALUE, start = c(year(st.date), month(st.date)), freq = n.freqency)
  }
  
  # quarterly frequency
  if (70 < time.diff & time.diff <= 100){
    n.freqency = 4
    x <- ts(xx$VALUE, start = c(year(st.date), quarter(st.date)), freq = n.freqency)
  }
  
  # yearly frequency
  if (300 < time.diff & time.diff <= 400){
    n.freqency = 1
    x <- ts(xx$VALUE, start = year(st.date), freq = n.freqency)
  }
  
  return(x)
  
}


#  Usage

# vector <- "v41690914"
# vector <- "v121820" # weekly
# vector <- "v39050" # daily
# start <- "2010-01-01"

# Daily Overnight money marekt interest rate

# i_ov <- ts_cansim('v39050', start = '1990-01-01')
# plot(i_ov)
# tail(i_ov)
# class(i_ov)
# tail(time(i_ov))
# View(i_ov)
# 
# housing.starts <- ts_cansim('v52300157', start = '1950-01-01')
# plot(housing.starts)
# 
# 
# consumption <- ts_cansim('v62305723', start = '1990-01-01')