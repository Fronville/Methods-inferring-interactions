library(data.table)
library(dplyr)
library(sp)
library(ggplot2)
library(gridExtra)
library(scales)
library(rgdal)
library(raster)
library(amt)

library(wildlifeDI)
library(adehabitatLT)
library(adehabitatHR)
library(adehabitatHS)


Example_data <- read.csv("example_data.csv")



#------------#
### SSF OD ###
#------------#

Example_data$Date <- Sys.time() + Example_data$Date

Example_data %>% ggplot(aes(x, y, col = as.factor(ID))) + geom_path()

dat1_pp <- Example_data %>% make_track(x, y, Date, id = ID, crs = 3035)

trast <- raster(amt::bbox(dat1_pp, buffer = 1), res = 1)

dat2_pp <- filter(dat1_pp, id == "prey")

od_roll <- rolling_od(dat2_pp, trast, n.points=10, show.progress = F)

m1 <- dat1_pp %>% 
  filter(id == "pred") %>% steps() %>% random_steps(n_control = 30) %>%
  extract_covariates_var_time(od_roll, max_time = minutes(5), when = "before",name_covar = "opponent")%>%
  filter(!is.na(opponent)) %>% fit_ssf(case_ ~ opponent + strata(step_id_))


#--------------#
### SSF DIST ###
#--------------#

Example_data <- read.csv("example_data.csv")


ft <- subset(Example_data, Example_data$ID == "prey")
rest <- subset(Example_data, Example_data$ID == "pred") 
track_ft <-  Example_data %>% make_track(x, y, Date, crs = 3035) 
rasti <- raster(amt::bbox(track_ft, buffer = 1), res = 3)


# for every timestep of focal, calculate distance of other cons to grid
for (k in 1:nrow(ft)){                                                                                                       
  cons <- subset(rest, rest$Date %in% ft$Date[k])           
  curr_ras_toadd <- raster::distanceFromPoints(rasti, cons[,c("x","y")])
  rasti <- addLayer(rasti,curr_ras_toadd)
}


cr <- rasti
t1 <- ft %>% make_track(x, y, Date, crs = 3035) 

m1 <- t1 %>% steps() %>% random_steps(n_control = 50) %>% extract_covariates(cr) # make random steps for ssf and extract covariates (distance rasters)

# only keep covariates of raster layer at respective time stamp
step_i = 1
m1$opponent <- NA
for (step_i in 1:nrow(m1)){
  m1$opponent[step_i] <- as.numeric(m1[step_i, 11 + m1$step_id_[step_i]])
}

m1_cut <- m1[,c(1:11, ncol(m1))]

m1_summary <- m1_cut %>% fit_ssf(case_ ~ opponent + strata(step_id_))


#-----------------#
### Wildlife DI ###
#-----------------#

Example_data <- read.csv("example_data.csv")

Example_data$Date <-  round.POSIXt(Sys.time(), units = "sec") +  ( Example_data$Date )

litrI <- as.ltraj(Example_data[,c("x","y")], id = Example_data$ID, date = Example_data$Date,  typeII = TRUE)
prey <- litrI[2]
predator <- litrI[1]

DI_m1<- DI(prey, predator, tc=7.5*60)
Cr_m1<- Cr(prey, predator, tc=7.5*60)
Cs_m1<- Cs(prey, predator, tc=7.5*60)
