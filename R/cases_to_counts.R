
build_aggregate_stmt <- function(
	source_table,
	group_by,
	from_timepoint,
	to_timepoint,
	delivery_timepoint
) {
	stmt <- paste0(
		"SELECT ", paste0(group_by, collapse=', '), ", count(*) FROM ",
		source_table, " WHERE ",
			"NOT date_sick < '", as.character(from_timepoint), "' AND ",
			"    date_sick < '", as.character(to_timepoint), "' AND ",
			"(delivery_date <= '", as.character(delivery_timepoint), "' ",
			" OR delivery_date IS NULL) ",
		"GROUP BY ", paste0(group_by, collapse=', '), " ",
		"ORDER BY ", paste0(group_by, collapse=', '), ";"
	)
	return(stmt)
}

fetch_cases <- function(link, stmt) {
	case_counts <- dbGetQuery(link[['conn']], stmt)
	return(case_counts)
}

aggregate_to_formula <- function(
	counts, formula,
	label_function = function(x) {
		x[['date_sick_biweek']] <- date_to_biweek(x[['date_sick']])
		return(x)
	}
) {
	counts[['date_sick_year']] <- year(counts[['date_sick']])
	split_counts <- split(x=counts, f=counts[['date_sick_year']], drop=FALSE)
	split_counts <- mclapply( X=split_counts, FUN=label_function, mc.cores=getOption("mc.cores",12))
	counts <- do.call(what=rbind, args=split_counts)
	case_counts <- aggregate(formula = formula, data = counts, FUN=sum)
	return(case_counts)
}

import_case_counts <- function(
	source_table = 'unique_case_data',
	group_by = c('disease','date_sick','province'),
	from_timepoint = now() - years(100), ## FROM (open)
	to_timepoint = now(), ## TO (closed)
	delivery_timepoint = now(),
	aggregate_formula = count ~ disease + province + date_sick_biweek + date_sick_year,
	link = stop("Need a link")
) {
	stmt <- build_aggregate_stmt(source_table, group_by, from_timepoint, to_timepoint, delivery_timepoint)
	case_counts <- fetch_cases(link, stmt)
	if(!is.null(aggregate_formula)) {
		agg_case_counts <- aggregate_to_formula(counts=case_counts, formula=aggregate_formula)
		return(agg_case_counts)
	} else {
		return(case_counts)
	}
}

import_old_case_counts <- function(
	source_table = 'biweekly_dengue_counts_1968_to_2005',
	link = stop("Need a link.")
) {
	stmt <- paste0("SELECT * FROM ", source_table, ";")
	old_counts <- dbGetQuery(link$conn, stmt)
	old_counts[['disease']] <- 26
	old_counts[['count']] <- old_counts[['smooth_counts']]
	old_counts[['smooth_counts']] <- NULL
	old_counts[['smooth_cumulative_counts']] <- NULL
	old_counts[['date_sick_year']] <- old_counts[['year']]
	old_counts[['year']] <- NULL
	old_counts[['times']] <- NULL
	return(old_counts)
}

##' 
##' Function for joining old and new case counts. Aggregates biweeks and then
##' merges...accepting the new data over the old in all cases
##' 
##' @param new_counts the new case counts
##' @param old_counts the new case countes
##' 
##' @return an aggregated and merged data.frame
##' 
joint_old_new_cases <- function(new_counts, old_counts) {
  require(dplyr)
  
  #aggregate new counts
  new_counts_ag <- new_counts %>% 
    group_by(disease, date_sick_biweek, date_sick_year, province) %>%
    summarize(count=sum(count))
  
  
  #aggregate old counte
  old_counts_ag <- old_counts %>% 
    group_by(disease, date_sick_biweek, date_sick_year, province) %>%
    summarize(count=sum(count))
  
  #merge....favoring new counts
  tmp1<- make.dpyw.uid(new_counts_ag)
  tmp2<- make.dpyw.uid(old_counts_ag)
  tmp <-which(!tmp2%in%tmp1)
  tot_dat <- rbind(new_counts_ag, old_counts_ag[tmp,])
  
  return(tot_dat)
}

##' Utility function for making UIDs based on disease province, biweek and year from data from 
##' the dengue db. 
##' 
##' @param data the data, must have columns disease, province, date_sick_biweek, and date_sick_year
make.dpyw.uid <- function(data) {
  uid <- as.numeric(data$disease)*100*10000*100+ as.numeric(data$province)*100*10000+ data$date_sick_year*100 + data$date_sick_biweek
  return(uid)
}


	
## Example: 
#new_counts <- import_case_counts(link=link)
#old_counts <- import_old_case_counts(link=link)
#counts <- joint_old_new_cases(new_counts, old_counts)






	
