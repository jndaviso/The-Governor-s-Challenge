# ts_fred 
# repurposed ts_cansim to do the same thing for fred data

ts_fred <- function(series_id , start, end = NA){
  
  # packages
  require(fredr)
  require(lubridate)
  
  # get series from fred
  if(is.na(end)){                                                         
    xx <- fredr(series_id , observation_start = as.Date(start))                 
  } else {                                                                
    xx <- fredr(series_id , observation_start = as.Date(start), observation_end = as.Date(end))  
  } 
  
  # determine frequency
  time.diff <- as.Date(xx$date[2]) - as.Date(xx$date[1]) 

  x <- xx$value     # set x to be the "value" column from xx
  n <- nrow(xx)     # returns the number of rows in xx

  st.date <- as.Date(xx$date[1])    # set the start date variable as the "ref_date" column in xx, first entry
  end.date <- as.Date(xx$date[n])

  # daily frequency
  if (1 <= time.diff & time.diff <= 4){
    n.freqency = 365.25
    x <- ts(xx$value, start = c(year(st.date), month(st.date), day(st.date)), freq = n.freqency)
  } 
  
  # weekly frequency
  if (time.diff == 7){
    n.freqency = 365.25/7
    x <- ts(xx$value, start = c(year(st.date), month(st.date), day(st.date)), freq = n.freqency)
  }
  
  # monthly frequency 
  if (25 < time.diff & time.diff <= 31){
    n.freqency = 12
    x <- ts(xx$value, start = c(year(st.date), month(st.date)), freq = n.freqency)
  }
  
  # quarterly frequency
  if (70 < time.diff & time.diff <= 100){
    n.freqency = 4
    x <- ts(xx$value, start = c(year(st.date), quarter(st.date)), freq = n.freqency)
  }
  
  # yearly frequency
  if (300 < time.diff & time.diff <= 400){
    n.freqency = 1
    x <- ts(xx$value, start = year(st.date), freq = n.freqency)
  }

  return(x)
  
}

# usage

# series_id <- "MEXCPIALLMINMEI" 
# start <- "2010-01-01" may need to format as as.Date

# usgdp <- ts_fred("GDPC1", start= '1990-01-01')
  





