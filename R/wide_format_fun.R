

create_standard_wide_format <- function(case_counts, 
                                        keep_codes = c('26','27','66'),
                                        path_to_census = '2010Census.csv') {
	case_counts <- case_counts[case_counts[['disease']] %in% keep_codes,]
	case_counts[['disease']] <- NULL   ## Standard format has no notion of count by disease type.
        count_melt <- melt(data=case_counts, 
                           id.vars=c('province','date_sick_year','date_sick_biweek'), 
                           measure.vars='count')
       	## standardize 15 day biweek case counts (always biweek 26, biweek 5 in leap years)
       	idx_bw_26 <- which(count_melt$date_sick_biweek==26)
       	leap_years <- seq(1904, 2096, by=4)
       	idx_bw_5 <- which(count_melt$date_sick_biweek==5 & count_melt$date_sick_year %in% leap_years)
       	idx_to_adjust <- c(idx_bw_26, idx_bw_5)
       	count_melt[idx_to_adjust, "value"] <- count_melt[idx_to_adjust, "value"]*14/15

       	## make into wide format
        standard_count_format <- dcast(data=count_melt, formula=province ~ date_sick_year + date_sick_biweek,
                                       fun.aggregate=sum)
	standard_count_format <- standard_count_format[order(standard_count_format[['province']]),]
	
	## Extra data
	census <- read.csv(file=path_to_census, header=TRUE, skip=1)
	colnames(census) <- c('names','HASC','code','FIPS','region','pop.2000','area_km','area_miles')
	
	census <- census[order(census[['code']]),c('names','code','FIPS','pop.2000')]
	if (!all(census[['ISO']] == standard_count_format[['province']])) 
		stop("Not all provinces appear in the census.")
	
	counts <- cbind(census,standard_count_format)
	counts[['province']] <- NULL
	
	## For rows
	split_time <- strsplit(x=colnames(counts)[5:ncol(counts)], split='_')
	year <- sapply(split_time, function(x) as.numeric(x[1]))
	biweek <- sapply(split_time, function(x) as.numeric(x[2]))
	time <- year + (biweek-1)/26
	
	## For colanmes
	biweek_string <- formatC(x=biweek, width=2, flag="0")
	year_string <- sapply(year, function(x) substr(x=as.character(x),start=3,stop=4))
	colnames(counts) <- c(colnames(counts)[1:4],paste0('BW',biweek_string,'.',year_string))
	
	# Vectors: code, fips, pop.2000
	province_names <- as.character(counts[['names']])
	code <- counts[['code']]
	fips <- counts[['FIPS']]
	pop  <- counts[['pop.2000']]
	
	# Matrix: counts
	count_matrix <- as.matrix(counts[,5:ncol(counts)])
	
	# Matrix: 'year','time.in.year','time'
	time_matrix <- as.matrix(data.frame(year=year, time.in.year=biweek, time=time))
	
	return(list(
		province_names = province_names,
		code = code,
		fips = fips,
		pop = pop, 
		count_matrix = count_matrix,
		time_matrix = time_matrix
	))
}
	

#	save(province_names, code, fips, pop, count_matrix, time_matrix, file=path_to_output)


