# ==========================
# clear workspace
rm(list=ls())
dev.off()

library(easypackages)
libraries("acs", "foreign", "dplyr", "tidyr", 
           "xlsx", "qdap", "dataframes2xls", "stringr")



###########################################################################################

###########################################################################################

###########################################################################################


##  You'll need to set the workspace and API key below to your own for this to work. 

setwd("SET TO YOUR OWN FILE PATH")

#Replace this with the location of your tract shapefile's DBF
dat = read.dbf("INPUT_Study_Area_Tract_Intersected.dbf",as.is = TRUE) 


##  ##  ##  ##

api.key.install(key="ENTER YOUR OWN API KEY HERE")

##  ##  ##  ##

# ACS requires a geo object for its search. 
# Be careful that FIPS loaded as character, with the following"
# 2 digit state, 3 digit county, 6 digit tract

geo<-geo.make(state  = as.integer(as.list(dat$STATEFP10)),
              county = as.integer(as.list(dat$COUNTYFP10)), 
              tract  = as.integer(as.list(dat$TRACTCE10)))


#Fetch data.
ACS_Rental_Units<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                        table.number = "B25042", col.names = "pretty")
attr(ACS_Rental_Units, "acs.colnames")


#Convert from ACS object to a normal data frame. 
ACS_Rental_Units <- data.frame(ACS_Rental_Units@geography, ACS_Rental_Units@estimate)


#Subsetting the dataset to just the fields that I need and renaming them. 
ACS_Rental_Units <- ACS_Rental_Units[13:19]
colnames(ACS_Rental_Units) <- c("Units", "Studios", "1_BR", "2_BR", "3_BR", "4_BR", "5_BR")


#  Totaling the ACS data and changing to wide format with gather. 
ACS_Rental_Units <- ACS_Rental_Units %>% summarise_each(funs(sum))
ACS_Rental_Units  <- gather(ACS_Rental_Units)
rownames(ACS_Rental_Units) <- ACS_Rental_Units[,1]

ACS_Rental_Units   <- spread(ACS_Rental_Units , key = key, value = value)



###########################################################################################

###########################################################################################

###########################################################################################



#Fetch data.
Less_4_Units <-acs.fetch(endyear = 2015, span = 5, geography = geo,
                            table.number = "B25032", col.names = "pretty")
attr(Less_4_Units, "acs.colnames")
#Convert from ACS object to a normal data frame. 
Less_4_Units <- data.frame(Less_4_Units@geography, Less_4_Units@estimate)


#Subsetting the dataset to just the fields that I need and renaming them. 
Less_4_Units <- Less_4_Units[c(17:21)]
colnames(Less_4_Units) <- c("Units", "1_detached", "1_attached", "2_Units", "3or4_Units")


Less_4_Units<- Less_4_Units %>% summarise_each(funs(sum))



###########################################################################################

###########################################################################################

###########################################################################################



#Fetch data.
ACS_Rent_by_BR<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                            table.number = "B25068", col.names = "pretty")
attr(ACS_Rent_by_BR, "acs.colnames")



#Convert from ACS object to a normal data frame. 
ACS_Rent_by_BR <- data.frame(ACS_Rent_by_BR@geography, ACS_Rent_by_BR@estimate)

#At this point we can clean these up and remove them.
rm(dat, geo)

# Subsetting by headers containing a string (as opposed to by column range.)
ACS_Rent_by_BR <- ACS_Rent_by_BR[grepl('cash', names(ACS_Rent_by_BR))]


# Subsetting one more time by headers containing a string (as opposed to by column range.)
ACS_Rent_by_BR <- ACS_Rent_by_BR[grepl('to|No.cash', names(ACS_Rent_by_BR))]


# Totaling Census tracts and then reshaping to long format with gather. 
ACS_Rent_by_BR<- ACS_Rent_by_BR %>% summarise_each(funs(sum))
ACS_Rent_by_BR <- gather(ACS_Rent_by_BR)

# Cleaning up the string fields a little.
ACS_Rent_by_BR$key <- gsub(".*Rent..","", ACS_Rent_by_BR$key )

# Splitting the string field into 2 groups that will be the 2 dimensions of the final matrix. 
####    Bedrooms and Rent. 
temp <- data.frame(str_split_fixed(ACS_Rent_by_BR$key, "cash.rent", 2))
ACS_Rent_by_BR <- bind_cols(temp,ACS_Rent_by_BR[-1])
rm(temp)

# Again, cleaning up the string fields a little.
ACS_Rent_by_BR$X1 <- gsub("..With.|..No.","", ACS_Rent_by_BR$X1 )

# Reshaping the data from long format into a 2x2 matrix using the acast( ) function. 
ACS_Rent_by_BR <- data.frame(acast(ACS_Rent_by_BR, X1 ~ X2))


###########################################################################################

###########################################################################################


list <- 'ACS_Rent_by_BR,ACS_Rental_Units,afford_rents,Less_4_Units'

write.xls(c(ACS_Rent_by_BR, ACS_Rental_Units,
            afford_rents, Less_4_Units), row.names = TRUE,
          sh.names =  list, "Charlotte_Rent_Inventory.xls")

