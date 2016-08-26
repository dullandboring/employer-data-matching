# this is a code example file used to locate specific establishments in administrative data.
# the code example uses the R stringdist library, computes a variety of measures, and then
# composites them.  compositing string distance measures helps to account for the fact that
# no one measure is ideal for every possible set of strings you may be matching against, and 
# leveraging the product of multiple measurements will often yield superior results.

# the approach illustrated below is typcially used to loop through lists of establishments.
# However, the code has been adapted to illustrate the methods in a simple, linear way.
# the code can easily be adapted, optimized and configured for a variety of tasks.

# note also that the data below is publicly available data from DOL's public enforcement data
# site.  (ref: http://ogesdw.dol.gov/views/data_catalogs.php)  the example draws from public 
# data on occupational safety and health inspections.  the context in which we would use this 
# is finding all instances of a firm or establishment to follow firms longitudinally through time.

library(stringdist)
library(stringr)
library(png)

#========================

setwd("C:\\scott\\enforcement")

# read in OSHA data
#oshadata <- read.csv("WORK_QUERY_FOR_OSHA_INSPECTION_2.csv")  # read csv file 
#save(oshadata, file = "oshadata.rdata")
#head(oshadata)
load("oshadata.rdata")

#========================

# remove some unnecessary fields to free up memory
# we are keeping inspection number, establishment name, street address, city, state, and NAICS for confirmation

oshadata <- oshadata[-c(2,7,9)]

# some code to do minor QA/QC and to maximize matching efficiency 
oshadata$estab_name <- str_to_upper(str_trim(oshadata$estab_name, side = c("both")))
oshadata$estab_address <- str_to_upper(str_trim(oshadata$site_address, side = c("both")))
oshadata$estab_city <- str_to_upper(str_trim(oshadata$site_city, side = c("both")))
oshadata$estab_st <- str_to_upper(str_trim(oshadata$site_state, side = c("both")))

# ================================

## below is the establishment we are seeking to find in the administrative data.
## minor QA on the fields to make comparable, maximize matching efficiency

xa <- toString(str_to_upper(str_trim("Gavilon Grain LLC, dba Peavey Grain")))

# create empty container(s) template to host results
osha_dist <- data.frame(matrix(ncol = 0, nrow = nrow(oshadata)))
osha_dist2 <- osha_dist

# ================================

# begin computing possible string distances
# each method has four steps, compute string distance, compute mean and sd, compute standardized value

osha_dist$estab_dist_lv <- stringdist(oshadata$estab_name,xa,method = 'lv')  
dist_mean <- mean(osha_dist$estab_dist_lv)
dist_sd <- sd(osha_dist$estab_dist_lv)
osha_dist2$estab_dist_lv <- (((osha_dist$estab_dist_lv) - dist_mean ) / dist_sd)

osha_dist$estab_dist_osa <- stringdist(oshadata$estab_name,xa,method = 'osa')  
dist_mean <- mean(osha_dist$estab_dist_osa)
dist_sd <- sd(osha_dist$estab_dist_osa)
osha_dist2$estab_dist_osa <- (((osha_dist$estab_dist_osa) - dist_mean ) / dist_sd)

osha_dist$estab_dist_dl <- stringdist(oshadata$estab_name,xa,method = 'dl')  
dist_mean <- mean(osha_dist$estab_dist_dl)
dist_sd <- sd(osha_dist$estab_dist_dl)
osha_dist2$estab_dist_dl <- (((osha_dist$estab_dist_dl) - dist_mean ) / dist_sd)

# note: hamming exlcuded because it's defined here for strings of identical length and that will rarely be the case in this context.  it will return N/A values in almost all cases...
#osha_dist$estab_dist_hamming <- stringdist(oshadata$estab_name,xa,method = 'hamming')  
#dist_mean <- mean(osha_dist$estab_dist_hamming)
#dist_sd <- sd(osha_dist$estab_dist_hamming)
#osha_dist2$estab_dist_hamming <- (((osha_dist$estab_dist_hamming) - dist_mean ) / dist_sd)

osha_dist$estab_dist_lcs <- stringdist(oshadata$estab_name,xa,method = 'lcs')  
dist_mean <- mean(osha_dist$estab_dist_lcs)
dist_sd <- sd(osha_dist$estab_dist_lcs)
osha_dist2$estab_dist_lcs <- (((osha_dist$estab_dist_lcs) - dist_mean ) / dist_sd)

osha_dist$estab_dist_qgram <- stringdist(oshadata$estab_name,xa,method = 'qgram')  
dist_mean <- mean(osha_dist$estab_dist_qgram)
dist_sd <- sd(osha_dist$estab_dist_qgram)
osha_dist2$estab_dist_qgram <- (((osha_dist$estab_dist_qgram) - dist_mean ) / dist_sd)

osha_dist$estab_dist_cosine <- stringdist(oshadata$estab_name,xa,method = 'cosine')  
dist_mean <- mean(osha_dist$estab_dist_cosine)
dist_sd <- sd(osha_dist$estab_dist_cosine)
osha_dist2$estab_dist_cosine <- (((osha_dist$estab_dist_cosine) - dist_mean ) / dist_sd)

osha_dist$estab_dist_jaccard <- stringdist(oshadata$estab_name,xa,method = 'jaccard')  
dist_mean <- mean(osha_dist$estab_dist_jaccard)
dist_sd <- sd(osha_dist$estab_dist_jaccard)
osha_dist2$estab_dist_jaccard <- (((osha_dist$estab_dist_jaccard) - dist_mean ) / dist_sd)

osha_dist$estab_dist_jw <- stringdist(oshadata$estab_name,xa,method = 'jw', p = 0.1)  
dist_mean <- mean(osha_dist$estab_dist_jw)
dist_sd <- sd(osha_dist$estab_dist_jw)
osha_dist2$estab_dist_jw <- (((osha_dist$estab_dist_jw) - dist_mean ) / dist_sd)

# ================================
# 
# there are some comments made on the computed scores below, after they are composited.  (184-192)
# if you want to look at the computed measures to get a sense of the magnitude and range....

# View(osha_dist2)

# ================================

# In order to decide which string distance measures are appropriate or practically helpful to composite,
# make a scatterplot of  correlations among the distance measures to get a sense of the relatedness
# we take a random sample, not because we need to but because our govt laptops don't 
# process large amounts of data very well
set.seed(123)
sampvect <- sample(1:nrow(osha_dist2), 5000)
osha_corr <- osha_dist2[sampvect, ]

# function from: https://www.r-bloggers.com/scatterplot-matrices-in-r/
# panel.cor will compute  correlation and put in upper panels, size proportional to correlation
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# output file to image in the current working directory
jpeg(filename = "corrplot.jpg", width = 1200, height = 916, units = "px", pointsize = 12)

# scatter plot matrix, one of many possible ways to implement this.
pairs(~estab_dist_lv+estab_dist_osa+estab_dist_dl+estab_dist_lcs+
        estab_dist_qgram+estab_dist_cosine+estab_dist_jaccard+estab_dist_jw, 
      data=osha_corr,
      lower.panel=panel.smooth, 
      upper.panel=panel.cor, 
      pch=20, 
      main="Correlation Among String Distance Measures")

dev.off()
dev.set(which = 1)


# ================================

# comment on scatterplot matrix:
# you can generally see that families of methods yield very high correlations.  
# in particular, the levenshtein, Damerau-Levenstein, and Optimal String Alignment 
# are highly co-linear, and composting their values will offer little additional benefit.

# for the example below, I will drop levenshtein and Optimal String Alignment, and use the rest.
# this is not optimal, particularly the correlations among the last three categories, but it will 
# suffice to illustrate the point

#composite the string distance measures we are using into an average, you could weight some measurements higher or lower if you choose base don experience with your domain data
osha_dist2$estab_avg <- ((as.numeric(osha_dist2$estab_dist_dl)+
                            as.numeric(osha_dist2$estab_dist_lcs)+
                            as.numeric(osha_dist2$estab_dist_qgram)+
                            as.numeric(osha_dist2$estab_dist_cosine)+
                            as.numeric(osha_dist2$estab_dist_jaccard)+
                            as.numeric(osha_dist2$estab_dist_jw)) / 6 )


#add in the actual establishment name and NAICS code for confirmation and clarity
osha_dist$estab <- oshadata$estab_name
osha_dist2$estab <- oshadata$estab_name
osha_dist2$naics <- oshadata$naics_code

# sort cases on average distance
osha_dist2 <- osha_dist2[order(osha_dist2$estab_avg),]

# if you look at the osha_dist2 file, you will see that all records are ranked by their 
# composite distance.  the records near the top should be the ones of interest.

# it's worth noting a couple of things based on the results:  

# 1) Highly related strings result in small distance measurement.  transformed or untransformed, 
#    this process will concentrate cases likely to be of interest near the top. 
# 2) Doing nothing other than capitalizing and removing leading or trailing blanks (see lines 38-41 of code),
#    this process will find virtually all of the cases you are looking for in spite of the variation
#    note that there are many different permutations of the company name we seek in rows 1-31.
#    there are also some DBAs which suggest additional search strings we may be interested in.
# 3) The establishment/firm name appears to have many permutations. DBA name and address searching or additional
#    entity resolution checks should be employed to find all instances of the firm. 
# 4) In spite of the nice finding in number 3, the reality is that we did not get all the cases we wanted.  
#    Rows 40-41 also have records we would have wanted to look at. this suggests a more general search pattern may be more effective.

# ==========================================================
# ==========================================================

# Below, I include some rough code that shows a few extensions of this very basic method.

# (1) search the alternate company names to find out if we really have all of the administrative records.
#     the example will include one of the DBA names we found earlier; Peavey Company, but you couls also use Grainstore or Peavy Grain

# (2) find all of the addresses of the records we identified and use similar methods to find other records 
#     with similar adddresses :: look to see if other company names are present or we missed alternate names

# ================================

# (1) search the alternate company names to ensure that we are not missing administrative records
#     the process is here is the same: we will search the company name field using multiple distance 
#     distance measures, standardize and composite them, and then sort/rank the results...

xa <- toString(str_to_upper("Peavey Company"))

# create empty container(s) template to host results
osha_dist <- data.frame(matrix(ncol = 0, nrow = nrow(oshadata)))
osha_dist2 <- osha_dist

# ================================

# begin computing possible string distances
osha_dist$estab_dist_dl <- stringdist(oshadata$estab_name,xa,method = 'dl')  
dist_mean <- mean(osha_dist$estab_dist_dl)
dist_sd <- sd(osha_dist$estab_dist_dl)
osha_dist2$estab_dist_dl <- (((osha_dist$estab_dist_dl) - dist_mean ) / dist_sd)

osha_dist$estab_dist_lcs <- stringdist(oshadata$estab_name,xa,method = 'lcs')  
dist_mean <- mean(osha_dist$estab_dist_lcs)
dist_sd <- sd(osha_dist$estab_dist_lcs)
osha_dist2$estab_dist_lcs <- (((osha_dist$estab_dist_lcs) - dist_mean ) / dist_sd)

osha_dist$estab_dist_qgram <- stringdist(oshadata$estab_name,xa,method = 'qgram')  
dist_mean <- mean(osha_dist$estab_dist_qgram)
dist_sd <- sd(osha_dist$estab_dist_qgram)
osha_dist2$estab_dist_qgram <- (((osha_dist$estab_dist_qgram) - dist_mean ) / dist_sd)

osha_dist$estab_dist_cosine <- stringdist(oshadata$estab_name,xa,method = 'cosine')  
dist_mean <- mean(osha_dist$estab_dist_cosine)
dist_sd <- sd(osha_dist$estab_dist_cosine)
osha_dist2$estab_dist_cosine <- (((osha_dist$estab_dist_cosine) - dist_mean ) / dist_sd)

osha_dist$estab_dist_jaccard <- stringdist(oshadata$estab_name,xa,method = 'jaccard')  
dist_mean <- mean(osha_dist$estab_dist_jaccard)
dist_sd <- sd(osha_dist$estab_dist_jaccard)
osha_dist2$estab_dist_jaccard <- (((osha_dist$estab_dist_jaccard) - dist_mean ) / dist_sd)

osha_dist$estab_dist_jw <- stringdist(oshadata$estab_name,xa,method = 'jw', p = 0.1)  
dist_mean <- mean(osha_dist$estab_dist_jw)
dist_sd <- sd(osha_dist$estab_dist_jw)
osha_dist2$estab_dist_jw <- (((osha_dist$estab_dist_jw) - dist_mean ) / dist_sd)

# ================================

#composite the measurement into an average, you could weight some measurements if you choose
osha_dist2$estab_avg <- ((as.numeric(osha_dist2$estab_dist_dl)+
                            as.numeric(osha_dist2$estab_dist_lcs)+
                            as.numeric(osha_dist2$estab_dist_qgram)+
                            as.numeric(osha_dist2$estab_dist_cosine)+
                            as.numeric(osha_dist2$estab_dist_jaccard)+
                            as.numeric(osha_dist2$estab_dist_jw)) / 6 )


#add in the actual establishment name and NAICS code for confirmation and clarity
osha_dist$estab <- oshadata$estab_name
osha_dist2$estab <- oshadata$estab_name
osha_dist2$naics <- oshadata$naics_code

# sort cases on average distance
osha_dist2 <- osha_dist2[order(osha_dist2$estab_avg),]

# This turns up two records: 372925, and 371490, neither of which were part of our earlier list.
# when we check the NAICS code, it's 424510 which resolves to "Grain and Field Bean Merchant Wholesalers"
# this is strongly suggestive that we have an administraitve record that we are lookign for.

# To further confirm, we can find out if the address from these records match any of the other establishments...
a_cases <- c(372925, 371490)
oshadata2 <- oshadata[a_cases, ]

#The results should look like this, note that the establishment name spelling is different, but both were found
# 372925  331076109	PEAVEY COMPANY	4256 54TH AVENUE NORTH	GRAND FORKS	ND	GRAND FORKS	ND
#	371490	330984451	PEAVY COMPANY	3330 112TH AVENUE SE	VALLEY CITY	ND	VALLEY CITY	ND

# None of the prior records were in the state of North Dakota but the NAICS code matches so a decison can be made regarding 
# whether these records are appropriate to include.  without this process, it is unlikely these records would be found.

# this was the list of inspection numbers that we identified in the first section of code.  
# we can use it as a point of comparison.
a_cases <- c(120198,120207,57470,72425,31922,57485,8970,207253,144128,144129,11333,28444,
             249058,402780,409385,409407,425887,431662,453150,459130,462313,464898,498684,
             546743,28436,368154,368990,419833,468806,578381,517942,442231,582789)

oshadata3 <- oshadata[a_cases, ]


# clean up a bit, free up some memory
rm(oshadata2)
rm(oshadata3)
rm(osha_dist)
rm(osha_dist2)
rm(osha_dist3)
rm(dist_mean)
rm(dist_sd)

# (2)  perform an address search to see if there are other additional or unknown aliases at the same address
#      this will follow the general pattern described above, but will perform the string distance computations 
#      and compositing on each of the address elements.  This is not perfectly ideal, but is meant to illustrate 
#      somethign akin to an ensemble approach, using multiple measures among multiple components, and producing 
#      an overall measure.  

#      as a quick side note, an alternate to this is to assign a physical location to each address, and then compute 
#      the physical distance between each location, and composite that as you would any other string distance measurement.
#      the scales will will be quite different, but if you standardize the data, the largest negative values should
#      continue to indicate the greatest proximity to the search string/location of interest

#      finally, note that the most common use of this method is finding vastly different establishment
#      names that really reflect the same firm/establishment.  put another way, this is more about getting around potential
#      data quality issues, than ensemble matching.  there is considerable research on blocking and data edits to 
#      better support matching, but these examples are meant to illustrate the act that many administrative data 
#      souces may need approaches beyond this.  note that "USPS", "US Postal Service", "United States Postal
#      Service" and "USPS-PROCESSING & DISTRIBUTION CENTER" all refer to the same establishment. In fact, this database 
#      has a dizzying variety of names for what is essentially the same establishment.  code snippet below illustrates: 

#              newdata <- grep("USPS", oshadata$estab_name)
#              a_cases <- c(newdata)
#              uspsdemo <- oshadata[a_cases, ]
#              view(uspsdemo)

#      So the basic approach demonstrated below is searching on the physical address and trying to return all 
#      possible variants and permutations on a company name.  This has the potential to pick up DBAs ("doing business as"),
#      successorship, and data quality issues at the point of data entry.  the example below shows how a single address
#      will yield the same establishment with very differnet names.  some of the string distance measures such as 
#      longest common substring and q gram measures might pick these up but the edit distance measures would miss most.


              newdata <- grep("USPS", oshadata$estab_name)
              a_cases <- c(newdata)
              uspsdemo <- oshadata[a_cases, ]
              view(uspsdemo)

# ================================

# input an address, city and state from the prior list of inspections, this can be automated,
# but the example here is manual for clarity
xb <- "4600 MARK IV PARKWAY"
xc <- "FORT WORTH"
xd <- "TX"

#xb <- "1124 PACIFIC STREET"
#xc <- "OMAHA"
#xd <- "NE"


# ================================
  
### perform the string comparisons on the establishment address

# create empty container(s) template to host results
osha_dist <- data.frame(matrix(ncol = 0, nrow = nrow(oshadata)))
osha_dist2 <- osha_dist  

osha_dist$estab_address_dl <- stringdist(oshadata$site_address,xb,method = 'dl')  
osha_dist$estab_address_dl[osha_dist$estab_address_dl =="Inf"] <- 99
dist_mean <- mean(osha_dist$estab_address_dl)
dist_sd <- sd(osha_dist$estab_address_dl)
osha_dist2$estab_address_dl <- (((osha_dist$estab_address_dl) - dist_mean ) / dist_sd)
  
osha_dist$estab_address_lcs <- stringdist(oshadata$site_address,xb,method = 'lcs')  
osha_dist$estab_address_lcs[osha_dist$estab_address_lcs =="Inf"] <- 99
dist_mean <- mean(osha_dist$estab_address_lcs)
dist_sd <- sd(osha_dist$estab_address_lcs)
osha_dist2$estab_address_lcs <- (((osha_dist$estab_address_lcs) - dist_mean ) / dist_sd)
  
osha_dist$estab_address_jaccard <- stringdist(oshadata$site_address,xb,method = 'jaccard')  
osha_dist$estab_address_jaccard[osha_dist$estab_address_jaccard =="Inf"] <- 99
dist_mean <- mean(osha_dist$estab_address_jaccard)
dist_sd <- sd(osha_dist$estab_address_jaccard)
osha_dist2$estab_address_jaccard <- (((osha_dist$estab_address_jaccard) - dist_mean ) / dist_sd)
  
osha_dist$estab_address_jw <- stringdist(oshadata$site_address,xb,method = 'jw')  
osha_dist$estab_address_jw[osha_dist$estab_address_jw =="Inf"] <- 99
dist_mean <- mean(osha_dist$estab_address_jw)
dist_sd <- sd(osha_dist$estab_address_jw)
osha_dist2$estab_address_jw <- (((osha_dist$estab_address_jw) - dist_mean ) / dist_sd)
  
osha_dist2$estab_avg <- ((as.numeric(osha_dist2$estab_address_dl)+
                              as.numeric(osha_dist2$estab_address_lcs)+
                              as.numeric(osha_dist2$estab_address_jaccard)+
                              as.numeric(osha_dist2$estab_address_jw)) / 4 )
  
  
  # collapse the data down to just average distance
  osha_dist2 <- osha_dist2[-c(1:4)]
  
  
  ########################################################
  ########################################################
  
# create empty container(s) template to host results
osha_dist <- data.frame(matrix(ncol = 0, nrow = nrow(oshadata)))
osha_dist3 <- osha_dist  
  
  osha_dist$city_dist_dl <- stringdist(oshadata$site_city,xc,method = 'dl')  
  osha_dist$city_dist_dl[osha_dist$city_dist_dl =="Inf"] <- 99
  dist_mean <- mean(osha_dist$city_dist_dl)
  dist_sd <- sd(osha_dist$city_dist_dl)
  osha_dist3$city_dist_dl <- (((osha_dist$city_dist_dl) - dist_mean ) / dist_sd)
  
  osha_dist$city_dist_lcs <- stringdist(oshadata$site_city,xc,method = 'lcs')  
  osha_dist$city_dist_lcs[osha_dist$city_dist_lcs =="Inf"] <- 99
  dist_mean <- mean(osha_dist$city_dist_lcs)
  dist_sd <- sd(osha_dist$city_dist_lcs)
  osha_dist3$city_dist_lcs <- (((osha_dist$city_dist_lcs) - dist_mean ) / dist_sd)
  
  osha_dist$city_dist_jaccard <- stringdist(oshadata$site_city,xc,method = 'jaccard')  
  osha_dist$city_dist_jaccard[osha_dist$city_dist_jaccard =="Inf"] <- 99
  dist_mean <- mean(osha_dist$city_dist_jaccard)
  dist_sd <- sd(osha_dist$city_dist_jaccard)
  osha_dist3$city_dist_jaccard <- (((osha_dist$city_dist_jaccard) - dist_mean ) / dist_sd)
  
  osha_dist$city_dist_jw <- stringdist(oshadata$site_city,xc,method = 'jw')  
  osha_dist$city_dist_jw[osha_dist$city_dist_jw =="Inf"] <- 99
  dist_mean <- mean(osha_dist$city_dist_jw)
  dist_sd <- sd(osha_dist$city_dist_jw)
  osha_dist3$city_dist_jw <- (((osha_dist$city_dist_jw) - dist_mean ) / dist_sd)
  
  osha_dist3$city_avg <- ((
    as.numeric(osha_dist3$city_dist_dl)+
      as.numeric(osha_dist3$city_dist_lcs)+
      as.numeric(osha_dist3$city_dist_jaccard)+
      as.numeric(osha_dist3$city_dist_jw)) / 4 )
  
  # collapse the data down to just average distance
  osha_dist3 <- osha_dist3[-c(1:4)]
  
  ########################################################
  
# create empty container(s) template to host results
osha_dist <- data.frame(matrix(ncol = 0, nrow = nrow(oshadata)))
osha_dist4 <- osha_dist
  
  osha_dist$state_dist_dl <- stringdist(oshadata$site_state,xd,method = 'dl')  
  osha_dist$state_dist_dl[osha_dist$state_dist_dl =="Inf"] <- 99
  dist_mean <- mean(osha_dist$state_dist_dl)
  dist_sd <- sd(osha_dist$state_dist_dl)
  osha_dist4$state_dist_dl <- (((osha_dist$state_dist_dl) - dist_mean ) / dist_sd)
  
  osha_dist$state_dist_lcs <- stringdist(oshadata$site_state,xd,method = 'lcs')  
  osha_dist$state_dist_lcs[osha_dist$state_dist_lcs =="Inf"] <- 99
  dist_mean <- mean(osha_dist$state_dist_lcs)
  dist_sd <- sd(osha_dist$state_dist_lcs)
  osha_dist4$state_dist_lcs <- (((osha_dist$state_dist_lcs) - dist_mean ) / dist_sd)
  
  osha_dist$state_dist_jaccard <- stringdist(oshadata$site_state,xd,method = 'jaccard')  
  osha_dist$state_dist_jaccard[osha_dist$state_dist_jaccard =="Inf"] <- 99
  dist_mean <- mean(osha_dist$state_dist_jaccard)
  dist_sd <- sd(osha_dist$state_dist_jaccard)
  osha_dist4$state_dist_jaccard <- (((osha_dist$state_dist_jaccard) - dist_mean ) / dist_sd)
  
  osha_dist$state_dist_jw <- stringdist(oshadata$site_state,xd,method = 'jw')  
  osha_dist$state_dist_jw[osha_dist$state_dist_jw =="Inf"] <- 99
  dist_mean <- mean(osha_dist$state_dist_jw)
  dist_sd <- sd(osha_dist$state_dist_jw)
  osha_dist4$state_dist_jw <- (((osha_dist$state_dist_jw) - dist_mean ) / dist_sd)
  
  osha_dist4$state_avg <- ((as.numeric(osha_dist4$state_dist_dl)+
                              as.numeric(osha_dist4$state_dist_lcs)+
                              as.numeric(osha_dist4$state_dist_jaccard)+
                              as.numeric(osha_dist4$state_dist_jw)) / 4 )
  
  # collapse the data down to just name and distance
  osha_dist4 <- osha_dist4[-c(1:4)]


oshafinal <- oshadata

oshafinal$estab_dist <- osha_dist2$estab_avg
oshafinal$city_dist <- osha_dist3$city_avg
oshafinal$state_dist <- osha_dist4$state_avg
oshafinal$comp_dist <- oshafinal$estab_dist+oshafinal$city_dist+oshafinal$state_dist 
oshafinal <- oshafinal[order(oshafinal$comp_dist),]



########################################################

### the following code will restrict the output to only those cases with the largest negative scores.
### this is helpful if you are looping through many possible matched cases, but caution should be taken 
### as the thresholds for identifying the shortest possible distances will vary depending on the 
### conntent of the strings for street address and city

#oshafinal <- oshafinal[-c(3:6)]
#oshafinal <- oshafinal[!oshafinal$comp_dist > min(oshafinal$comp_dist)-(min(oshafinal$comp_dist))/15,]
#oshafinal <- oshafinal[!oshafinal$comp_dist > min(oshafinal$comp_dist),]


