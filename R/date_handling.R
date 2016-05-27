
## Get time as year/month.
split_on_decimal <- function(x) strsplit(x=x,split='\\.')

year_decimal_to_decimal <- function(year) {
	split_year <- split_on_decimal(as.character(year))
	split_year <- sapply(split_year, function(x) {
  if (length(x) == 1)
    return(0.0)
  else
    return(as.numeric(paste0('0.',x[2])))
	})
	return(split_year)
}

year_fraction_to_month <- function(x) {
  x <- round(x*12+1)
  return(x)
}


date_to_subyear_interval_factory <- function(map, leap_year_map=map) {
  require(lubridate)
  # Remember these:
  regular_year_map = map; leap_year_map = leap_year_map

  # Function def:
  date_to_subyear_interval <- function(date) {
		lyi  <- sapply( leap_year(date), isTRUE)
		nlyi <- sapply(!leap_year(date), isTRUE)
		o <- vector(mode='numeric', length=length(date))
		o[ is.na(date)] <- NA
    o[!is.na(date)] <- yday(date[!is.na(date)])
		o[lyi] <- leap_year_map[o[lyi],'biweek']
		o[nlyi] <- regular_year_map[o[nlyi],'biweek']
		if (any(o > 26 | o < 1)) stop()
		return(o)
  }

  return(date_to_subyear_interval)
}


dat <- data.frame(biweeks = 1:26, num_days = 14)
dat[26,'num_days'] <- 15
dat[['num_days_leap_year']] <- dat[['num_days']]
dat[5,'num_days_leap_year'] <- 15

no_leap_year <- data.frame(
  julian_day = 1:365,
  biweek = rep(x=dat[['biweeks']], times=dat[['num_days']])
)

leap_year <- data.frame(
  julian_day = 1:366,
  biweek = rep(x=dat[['biweeks']], times=dat[['num_days_leap_year']])
)

date_to_biweek <- date_to_subyear_interval_factory(
  map = no_leap_year, leap_year_map = leap_year)

biweek_to_date <- function(biweek, year){
        require(lubridate)
        biweek.df <- data.frame(Biweek=biweek, Year=year)
        biweek.df$Leap <- leap_year(biweek.df$Year)
        biweek.df$Yr.Adj <- 0
        biweek.df$Yr.Adj <- biweek.df$Year + floor((biweek.df$Biweek - 1)/26)
        biweek.df$Bw.Adj <- biweek.df$Biweek - 26*floor((biweek.df$Biweek - 1)/26)
        biweek_map <- with(environment(date_to_biweek), leap_year_map)
        biweek.df$Bw.Day <- biweek_map$julian_day[match(biweek.df$Bw.Adj, biweek_map$biweek)]
        biweek.df$Date <- as.Date(biweek.df$Bw.Day-1, origin=paste0("2008-01-01"))
        year(biweek.df$Date) <- biweek.df$Yr.Adj
        return(biweek.df$Date)
}


batchify <- function(date, breaks) {
  if (length(date) == 0) stop("Pass at least one date to 'batchify'.")
  if (length(date) > 1) 
    sapply(date, batchify, breaks=breaks)
  else {
    batch <- which(date > breaks)
    if (length(batch) >= 1)
      return(max(batch))
    else if (identical(batch, vector(mode='integer',length=0)))
      return(0)
    else 
      stop("Something is really rotten in Denmark.")
  }
}





