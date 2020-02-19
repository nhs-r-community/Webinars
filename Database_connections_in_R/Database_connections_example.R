#Database connections using DBI and dplyr

#Load relevant packages, install them if you don't have them yet.
#install.packages("RSQLite")
library(DBI)
library(dplyr)
library(dbplyr)
library(NHSRdatasets)


######################################################################################
# Set up exercise database.  This will be held in memory, using RSQLite
# You would likely be connecting to something SQL Server, Oracle or other in practise
######################################################################################

# Load ae_attendance data.frame from the NHSRdatasets package
data("ae_attendances")

# Set up the database connection
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# Write to the database, but we'll call it ae_attends instead
dbWriteTable(con, "ae_attends", ae_attendances, overwrite=TRUE)

#Remove the imported data frame so we have to work on the database
rm(ae_attendances)

#######################################################################################


# What tables do we have in our database?


# We can list the fields in a table: 




# Lets get the distint values for the field "type"




# Lets count how many entries we have in each type




## Side point, the error message is an RSQLite thing.  You need to explicitly close the results, 
# or it does it for you with the warning.  This is not a problem though.



# Now lets see the entries for my organisations UHB / RRK, using a where clause




##################################
# Using dplyr instead
##################################

# Now lets declare ae_attends as a tibble for use with dplyr. We'll call it 'ae' to avoid confusion
# This will now be treated as an R data.frame/tibble, but it is still in the database



# Use glimpse to see the structure and data types




# Lets select just the org_code and attendances




# Lets replicate the SQL queries above, but using dplyr / dbplyr
# Counts by all organisations




# How many of these relate to my organisation, UHB = 'RRK'




# This is writing SQL for us.  We can see this using the show_query command




# Now, if you need to pull the data into R you can add the collect function in


  


##############################################################################################################

# Putting it all together  - Plus some R candy for those of you who like that sort of thing...

##############################################################################################################

# Let's take data from the database and plot the number of breaches in Type 1 organisation for March 2017:
# SQLite doesn't have a date format, it converts to strings and doubles, so need to work it out as first step

ref_date<-as.double(as.Date("2017-03-01"))
ref_date


library(Cairo) # this is a better graphics device. Worth using if you want custom fonts etc.
library(ggplot2)
library(scales) # I'm using this to add commas to thousands on y-axis

ae %>% 
  filter(period == ref_date & type=="1" & breaches >2000) %>%
  ggplot(aes(y=breaches, x= reorder(org_code, -breaches)))+
    geom_col(aes(fill=reorder(org_code, -breaches)), alpha=0.5, show.legend = FALSE)+
    geom_hline(aes(yintercept=4000), linetype="dashed", size=1.5, col="red")+
    geom_label(aes(12, 4000,label = "Big Arbitrary Red Line!", vjust = -0.5), col="Red")+
    scale_fill_viridis_d(alpha = 0.5) +
    labs(x="Trust Code", y="Breaches",
         title="Breaches at Type 1 A&E departments, during March 2017",
         subtitle="Chart displays organisations with > 2000 breaches")+
    scale_y_continuous(label=comma)+
    theme_minimal(base_family = "Roboto")+
    theme(axis.text.x=element_text(angle=90, hjust=1, size=8))

 

# And finally....
# Disconnect from the database
dbDisconnect(con)

