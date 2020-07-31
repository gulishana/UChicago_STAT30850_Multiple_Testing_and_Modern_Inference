download.file('https://data.sfgov.org/api/views/pyih-qa8i/rows.csv?accessType=DOWNLOAD','restaurantdata.csv')
data = read.table('restaurantdata.csv',sep=',',fill=TRUE,header=TRUE,comment.char='')

keep_inds = rep(TRUE,dim(data)[1])
# subset the data
# (1) keep only routine inspections
keep_inds[data$inspection_type!='Routine - Unscheduled'] = FALSE
# (2) remove any business ID that corresponds to multiple zip codes, or multiple latitude/longitude locations (this is a restaurant with multiple locations or errors in entry)
keep_inds[is.element(data$business_id,unique(data$business_id)[unlist(lapply(unique(data$business_id),function(x){length(unique(data$business_postal_code[data$business_id==x]))>1}))])] = FALSE
keep_inds[is.element(data$business_id,unique(data$business_id)[unlist(lapply(unique(data$business_id),function(x){length(unique(data$business_latitude[data$business_id==x]))>1}))])] = FALSE
keep_inds[is.element(data$business_id,unique(data$business_id)[unlist(lapply(unique(data$business_id),function(x){length(unique(data$business_longitude[data$business_id==x]))>1}))])] = FALSE

# cleaning - remove entries with data errors / missing data / etc
# risk category should be one of '' (empty = none), 'Low Risk','Moderate Risk','High Risk'
keep_inds[!is.element(data$risk_category,c('','Low Risk','Moderate Risk','High Risk'))] = FALSE
# if risk_category='' then should have violation_id='' and violation_description=''
keep_inds[(data$risk_category=='')&(data$violation_id!='')] = FALSE
keep_inds[(data$risk_category=='')&(data$violation_description!='')] = FALSE
# inspection score should be nonempty (integer 1 thru 100)
keep_inds[is.na(unlist(lapply(data$inspection_score,function(x){strsplit(toString(x),'')[[1]][1]})))] = FALSE
# zip code should start with a 9 and be five digits long
keep_inds[unlist(lapply(data$business_postal_code,function(x){strsplit(toString(x),'')[[1]][1]}))!='9'] = FALSE
keep_inds[unlist(lapply(data$business_postal_code,function(x){length(strsplit(toString(x),'')[[1]])})) != 5] = FALSE
# checking for problems in the date
keep_inds[!unlist(lapply(data$inspection_date,function(x){strsplit(toString(x),':')[[1]][3]=='00 AM'}))] = FALSE
keep_inds[is.na(unlist(lapply(data$inspection_date,function(x){strsplit(toString(x),':')[[1]][3]=='00 AM'})))] = FALSE
# business id should be 5 characters long
keep_inds[unlist(lapply(data$business_id,function(x){length(strsplit(toString(x),'')[[1]])})) != 5] = FALSE
# latitude & longitude should be numeric and in the right range
lat_tmp = as.numeric(unlist(lapply(data$business_latitude,function(x){strsplit(toString(x),':')[[1]][1]})))
long_tmp = as.numeric(unlist(lapply(data$business_longitude,function(x){strsplit(toString(x),':')[[1]][1]})))
keep_inds[((is.na(lat_tmp))|(35 > lat_tmp)|(40 < lat_tmp))] = FALSE
keep_inds[((is.na(long_tmp))|-125 > long_tmp)|(-120 < long_tmp)] = FALSE

# create clean data subset
n = sum(keep_inds)
data1 = data.frame(matrix(0,n,0))
row.names(data1) = which(keep_inds)
date_tmp = data[keep_inds,]$inspection_date
data1$month = as.numeric(unlist(lapply(date_tmp,function(x){strsplit(toString(x),'/')[[1]][1]})))
data1$day = as.numeric(unlist(lapply(date_tmp,function(x){strsplit(toString(x),'/')[[1]][2]})))
data1$year = as.numeric(unlist(lapply(date_tmp,function(x){strsplit(toString(x),'/| ')[[1]][3]})))
data1$business_id = as.numeric(strsplit(toString(data[keep_inds,]$business_id),',')[[1]])
data1$risk_category = strsplit(toString(data[keep_inds,]$risk_category),', ')[[1]]
data1$risk_category[data1$risk_category==''] = 'None'
data1$inspection_score = as.numeric(strsplit(toString(data[keep_inds,]$inspection_score),',')[[1]])
data1$zipcode = as.numeric(strsplit(toString(data[keep_inds,]$business_postal_code),',')[[1]])
data1$latitude = as.numeric(strsplit(toString(data[keep_inds,]$business_latitude),',')[[1]])
data1$longitude = as.numeric(strsplit(toString(data[keep_inds,]$business_longitude),',')[[1]])

# sort by date & restaurant ID
ord = order(data1$month*31+data1$day+(data1$year-2016)*366+data1$business_id*10000)
data1 = data1[ord,]

# save data
save(data1, file='data1.RData')
