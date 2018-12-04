#function to calculate growing degree-days (GDD) with a base temp of 8 C
GDD_8 <- function(max_temp,min_temp){
  ifelse(max_temp>307.15,mean(((317.15-seq(min_temp,max_temp,length.out=8))/10)*(307.15-281.15)),
         ifelse(max_temp>281.15 & min_temp<281.15,mean(seq(min_temp,max_temp,length.out=8)-281.15),
                ifelse((((max_temp-273.15)+(min_temp-273.15))/2)<8,0,
                       (((max_temp-273.15)+(min_temp-273.15))/2)-8)))
}

#function to calculate growing degree-days (GDD) with a base temp of 10 C
GDD_10 <- function(max_temp,min_temp){
  ifelse(max_temp>307.15,mean(((317.15-seq(min_temp,max_temp,length.out=8))/10)*(307.15-283.15)),
         ifelse(max_temp>283.15 & min_temp<283.15,mean(seq(min_temp,max_temp,length.out=8)-283.15),
                ifelse((((max_temp-273.15)+(min_temp-273.15))/2)<10,0,
                       (((max_temp-273.15)+(min_temp-273.15))/2)-10)))
}

#adding a GDD_8 dimension [variable = 7] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    for (k in 1:20454){
      A[7,j,k] <- GDD_8(A[1,j,k],A[2,j,k])
    }
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"historical simulation now has GDD_8 for",model[j]))
  }
}

#adding a GDD_8 dimension [variable = 7] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    for (k in 1:34333){
      A[7,j,k] <- GDD_8(A[1,j,k],A[2,j,k])
    }
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"RCP 4.5 now has GDD_8 for",model[j]))
  }
}

#adding a GDD_8 dimension [variable = 7] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    for (k in 1:34333){
      A[7,j,k] <- GDD_8(A[1,j,k],A[2,j,k])
    }
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"RCP 8.5 now has GDD_8 for",model[j]))
  }
}

#adding a GDD_10 dimension [variable = 7] to all the historical arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  #looping through the models
  for(j in 1:20){
    for (k in 1:20454){
      A[8,j,k] <- GDD_10(A[1,j,k],A[2,j,k])
    }
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_hist"),A)
    print(paste(locations$town[n],"historical simulation now has GDD_10 for",model[j]))
  }
}

#adding a GDD_10 dimension [variable = 7] to all the RCP 4.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  #looping through the models
  for(j in 1:20){
    for (k in 1:34333){
      A[8,j,k] <- GDD_10(A[1,j,k],A[2,j,k])
    }
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_45"),A)
    print(paste(locations$town[n],"RCP 4.5 now has GDD_10 for",model[j]))
  }
}

#adding a GDD_10 dimension [variable = 7] to all the RCP 8.5 arrays
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  #looping through the models
  for(j in 1:20){
    for (k in 1:34333){
      A[8,j,k] <- GDD_10(A[1,j,k],A[2,j,k])
    }
    #placing the results back into the array for each town
    assign(paste0(locations$town[n],"_85"),A)
    print(paste(locations$town[n],"RCP 8.5 now has GDD_10 for",model[j]))
  }
}

#date sequences
historical_dates <- as.Date(seq(18262,38715,1),origin="1900-01-01")
future_dates <- as.Date(seq(38716,73048,1),origin="1900-01-01")
#date sequences as day of the year
historical_doy <- as.integer(format(historical_dates,"%j"))
future_doy <- as.integer(format(future_dates,"%j"))

#function to calculate photoperiod (PP) which includes civil twilight
PP <- function(J,x,y){
  #solar declination
  solar_dec <- 0.409*sin(((2*pi/365)*J)-1.39)
  #converting to radians
  long <- (pi/180)*x
  lat <- (pi/180)*y
  #sunset time
  sunset <- (24/(2*pi))*((-long)-acos(((sin(lat)*sin(solar_dec))-sin(-0.10472))
                                      /(cos(lat)*cos(solar_dec))))
  #sunrise time
  sunrise <- (24/(2*pi))*((-long)+acos(((sin(lat)*sin(solar_dec))-sin(-0.10472))
                                       /(cos(lat)*cos(solar_dec))))
  #need to add 24 to sunset because negative
  #need to convert from UTC
  #quick and dirty timezone calculation from longitude using 360/24
  ((sunset-round(x/15))+24)-(sunrise-round(x/15))
}

#calculating historical and future PP for each location
for (n in 1:N){
  assign(paste0(locations$town[n],"_PP_hist"),
         PP(historical_doy,locations$lon[n],locations$lat[n]))
}
for (n in 1:N){
  assign(paste0(locations$town[n],"_PP_future"),
         PP(future_doy,locations$lon[n],locations$lat[n]))
}

#function to calculate the delay in tassel initiation
RATEIN <- function(delay,photoperiod){
  return(1/(4+delay*(photoperiod-12.5)))
}

#calculating historical and future RATEIN for each location
for (n in 1:N){
  B <-  get(paste0(locations$town[n],"_PP_hist"))
  assign(paste0(locations$town[n],"_RATEIN_hist"),RATEIN(G_2,B))
}
for (n in 1:N){
  B <-  get(paste0(locations$town[n],"_PP_future"))
  assign(paste0(locations$town[n],"_RATEIN_future"),RATEIN(G_2,B))
}

#year sequences to incorporate with planting dates
historical_years <- seq(1950,2004,1)
future_years <- seq(2006,2098,1)

#creating a vector for all planting dates
p_hist_e <- paste0(historical_years,planting_dates[1])
p_hist_m <- paste0(historical_years,planting_dates[2])
p_hist_l <- paste0(historical_years,planting_dates[3])
p_fut_e <- paste0(future_years,planting_dates[1])
p_fut_m <- paste0(future_years,planting_dates[2])
p_fut_l <- paste0(future_years,planting_dates[3])
#creating zoo objects
historical_zoo = read.zoo(as.data.frame(historical_dates),format = "%Y-%m-%d")
future_zoo = read.zoo(as.data.frame(future_dates),format = "%Y-%m-%d")
#setting reference indices
historical_index = index(historical_zoo)
future_index = index(future_zoo)
#starting what will become the planting date vectors
pd_hist_e <- 0
pd_hist_m <- 0
pd_hist_l <- 0
pd_fut_e <- 0
pd_fut_m <- 0
pd_fut_l <- 0
#looking up index values and adding to planting date vector
for (d in p_hist_e){
  pointer = which.min(abs(as.Date(d) - historical_index))
  pd_hist_e <- append(pd_hist_e,pointer,after=length(pd_hist_e))
}
for (d in p_hist_m){
  pointer = which.min(abs(as.Date(d) - historical_index))
  pd_hist_m <- append(pd_hist_m,pointer,after=length(pd_hist_m))
}
for (d in p_hist_l){
  pointer = which.min(abs(as.Date(d) - historical_index))
  pd_hist_l <- append(pd_hist_l,pointer,after=length(pd_hist_l))
}
for (d in p_fut_e){
  pointer = which.min(abs(as.Date(d) - future_index))
  pd_fut_e <- append(pd_fut_e,pointer,after=length(pd_fut_e))
}
for (d in p_fut_m){
  pointer = which.min(abs(as.Date(d) - future_index))
  pd_fut_m <- append(pd_fut_m,pointer,after=length(pd_fut_m))
}
for (d in p_fut_l){
  pointer = which.min(abs(as.Date(d) - future_index))
  pd_fut_l <- append(pd_fut_l,pointer,after=length(pd_fut_l))
}
#removing initial zero from series
pd_hist_e <- pd_hist_e[-1]
pd_hist_m <- pd_hist_m[-1]
pd_hist_l <- pd_hist_l[-1]
pd_fut_e <- pd_fut_e[-1]
pd_fut_m <- pd_fut_m[-1]
pd_fut_l <- pd_fut_l[-1]
#seed germination requires 1 day
pd_hist_e <- pd_hist_e + 1
pd_hist_m <- pd_hist_m + 1
pd_hist_l <- pd_hist_l + 1
pd_fut_e <- pd_fut_e + 1
pd_fut_m <- pd_fut_m + 1
pd_fut_l <- pd_fut_l + 1

#STAGE 1:planting to seedling emergence
#45 DTT_10
stage_1 <- 45

#STAGE 2:seedling emergence to end of juvenile phase
#P_1 DTT_8
#ranges from 110 to 390
stage_2 <- G_1

#STAGE 3:delay to tassel initiation
#sum RATEIN = 1
stage_3 <- 1

#STAGE 4:tassel emergence to silking
#62 DTT_8
stage_4 <- 62

#STAGE 5:lag phase (silking)
#170 DTT_8
stage_5 <- 170
#STAGE 6:silking to maturity
#0.95 * P_5
stage_6 <- 0.95*G_3

#function to calculate index locations of maize phenostages
#GDD_10, GDD_8, RATEIN
INDEXER <- function(start,vector_1,vector_2,vector_3){
  S_1 <- as.vector(sapply(
    start,
    function(x) which(cumsum(vector_2[x:length(vector_2)]) >= stage_1)[1]))
  S_2 <- as.vector(sapply(
    start + S_1,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_2)[1]))
  S_3 <- as.vector(sapply(
    start + S_1 + S_2,
    function(x) which(cumsum(vector_3[x:length(vector_3)]) >= stage_3)[1]))
  S_4 <- as.vector(sapply(
    start + S_1 + S_2 + S_3,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_4)[1]))
  S_5 <- as.vector(sapply(
    start + S_1 + S_2 + S_3 + S_4,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_5)[1]))
  S_6 <- as.vector(sapply(
    start + S_1 + S_2 + S_3 + S_4 + S_5,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_6)[1]))
  idx_1 <- start + S_1
  idx_2 <- start + S_1 + S_2
  idx_3 <- start + S_1 + S_2 + S_3
  idx_4 <- start + S_1 + S_2 + S_3 + S_4
  idx_5 <- start + S_1 + S_2 + S_3 + S_4 + S_5
  idx_6 <- start + S_1 + S_2 + S_3 + S_4 + S_5 + S_6
  string <- c(idx_1,idx_2,idx_3,idx_4,idx_5,idx_6)
  matrix(string,nrow=length(start),ncol=6)
}

#creating index arrays for each location (historical)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  B <-  get(paste0(locations$town[n],"_RATEIN_hist"))
  #creating an indexing array [model, year, stage, planting]
  index_array_hist <- array(dim=c(20,55,6,3))
  for(j in 1:20){
    matrix_1 <- INDEXER(pd_hist_e,A[7,j,],A[8,j,],B)
    matrix_2 <- INDEXER(pd_hist_m,A[7,j,],A[8,j,],B)
    matrix_3 <- INDEXER(pd_hist_l,A[7,j,],A[8,j,],B)
    index_array_hist[j,,,1] <- matrix_1
    index_array_hist[j,,,2] <- matrix_2
    index_array_hist[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_index_hist"),index_array_hist)
  #printing progress
  print(paste(locations$town[n],"maize stage index for historical simulation completed"))
}

#creating index arrays for each location (RCP 4.5)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  B <-  get(paste0(locations$town[n],"_RATEIN_future"))
  #creating an indexing array [model, year, stage, planting]
  index_array_fut <- array(dim=c(20,93,6,3))
  for(j in 1:20){
    matrix_1 <- INDEXER(pd_fut_e,A[7,j,],A[8,j,],B)
    matrix_2 <- INDEXER(pd_fut_m,A[7,j,],A[8,j,],B)
    matrix_3 <- INDEXER(pd_fut_l,A[7,j,],A[8,j,],B)
    index_array_fut[j,,,1] <- matrix_1
    index_array_fut[j,,,2] <- matrix_2
    index_array_fut[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_index_45"),index_array_fut)
  #printing progress
  print(paste(locations$town[n],"maize stage index for RCP 4.5 completed"))
}

#creating index arrays for each location (RCP 8.5)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  B <-  get(paste0(locations$town[n],"_RATEIN_future"))
  #creating an indexing array [model, year, stage, planting]
  index_array_fut <- array(dim=c(20,93,6,3))
  for(j in 1:20){
    matrix_1 <- INDEXER(pd_fut_e,A[7,j,],A[8,j,],B)
    matrix_2 <- INDEXER(pd_fut_m,A[7,j,],A[8,j,],B)
    matrix_3 <- INDEXER(pd_fut_l,A[7,j,],A[8,j,],B)
    index_array_fut[j,,,1] <- matrix_1
    index_array_fut[j,,,2] <- matrix_2
    index_array_fut[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_index_85"),index_array_fut)
  #printing progress
  print(paste(locations$town[n],"maize stage index for RCP 8.5 completed"))
}

#function to calculate number of days (span) to complete each stage
SPANNER <- function(start,vector_1,vector_2,vector_3){
  S_1 <- as.vector(sapply(
    start,
    function(x) which(cumsum(vector_2[x:length(vector_2)]) >= stage_1)[1]))
  S_2 <- as.vector(sapply(
    start + S_1,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_2)[1]))
  S_3 <- as.vector(sapply(
    start + S_1 + S_2,
    function(x) which(cumsum(vector_3[x:length(vector_3)]) >= stage_3)[1]))
  S_4 <- as.vector(sapply(
    start + S_1 + S_2 + S_3,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_4)[1]))
  S_5 <- as.vector(sapply(
    start + S_1 + S_2 + S_3 + S_4,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_5)[1]))
  S_6 <- as.vector(sapply(
    start + S_1 + S_2 + S_3 + S_4 + S_5,
    function(x) which(cumsum(vector_1[x:length(vector_1)]) >= stage_6)[1]))
  idx_1 <- S_1
  idx_2 <- S_1 + S_2
  idx_3 <- S_1 + S_2 + S_3
  idx_4 <- S_1 + S_2 + S_3 + S_4
  idx_5 <- S_1 + S_2 + S_3 + S_4 + S_5
  idx_6 <- S_1 + S_2 + S_3 + S_4 + S_5 + S_6
  string <- c(idx_1,idx_2,idx_3,idx_4,idx_5,idx_6)
  matrix(string,nrow=length(start),ncol=6)
}

#creating duration arrays for each location (historical)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_hist"))
  B <-  get(paste0(locations$town[n],"_RATEIN_hist"))
  #creating an indexing array [model, year, stage, planting]
  span_array_hist <- array(dim=c(20,55,6,3))
  for(j in 1:20){
    matrix_1 <- SPANNER(pd_hist_e,A[7,j,],A[8,j,],B)
    matrix_2 <- SPANNER(pd_hist_m,A[7,j,],A[8,j,],B)
    matrix_3 <- SPANNER(pd_hist_l,A[7,j,],A[8,j,],B)
    span_array_hist[j,,,1] <- matrix_1
    span_array_hist[j,,,2] <- matrix_2
    span_array_hist[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_span_hist"),span_array_hist)
  #printing progress
  print(paste(locations$town[n],"historical simulation maize stage duration completed"))
}

#creating duration arrays for each location (RCP 4.5)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_45"))
  B <-  get(paste0(locations$town[n],"_RATEIN_future"))
  #creating an indexing array [model, year, stage, planting]
  span_array_fut <- array(dim=c(20,93,6,3))
  for(j in 1:20){
    matrix_1 <- SPANNER(pd_fut_e,A[7,j,],A[8,j,],B)
    matrix_2 <- SPANNER(pd_fut_m,A[7,j,],A[8,j,],B)
    matrix_3 <- SPANNER(pd_fut_l,A[7,j,],A[8,j,],B)
    span_array_fut[j,,,1] <- matrix_1
    span_array_fut[j,,,2] <- matrix_2
    span_array_fut[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_span_45"),span_array_fut)
  #printing progress
  print(paste(locations$town[n],"RCP 4.5 maize stage duration completed"))
}

#creating duration arrays for each location (RCP 8.5)
for (n in 1:N){
  #copying the array of interest
  A <- get(paste0(locations$town[n],"_85"))
  B <-  get(paste0(locations$town[n],"_RATEIN_future"))
  #creating an indexing array [model, year, stage, planting]
  span_array_fut <- array(dim=c(20,93,6,3))
  for(j in 1:20){
    matrix_1 <- SPANNER(pd_fut_e,A[7,j,],A[8,j,],B)
    matrix_2 <- SPANNER(pd_fut_m,A[7,j,],A[8,j,],B)
    matrix_3 <- SPANNER(pd_fut_l,A[7,j,],A[8,j,],B)
    span_array_fut[j,,,1] <- matrix_1
    span_array_fut[j,,,2] <- matrix_2
    span_array_fut[j,,,3] <- matrix_3
  }
  #assinging an array for each location
  assign(paste0(locations$town[n],"_span_85"),span_array_fut)
  #printing progress
  print(paste(locations$town[n],"RCP 8.5 maize stage duration completed"))
}
