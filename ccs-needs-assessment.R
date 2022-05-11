# CCS needs assessment mapping
# Amelia Hoover Green
# First version: 2020
# This version: 2022

library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)

### 1. get variable codes ###
### NOTE: this section doesn't need to be run 
### unless we want to find new variables

# vars <- load_variables(2019,'acs5') %>% 
#   rename(variable=name) %>%
#   mutate(table=substr(variable,1,6),
#          label=gsub("Estimate!!Total",'',label,fixed=TRUE),
#          label=gsub('!!','',label,fixed=TRUE),
#          label=gsub(' ','',label,fixed=TRUE),
#          label=gsub('-','_',label,fixed=TRUE),
#          label=gsub(':','_',label,fixed=TRUE),
#          newname=paste(gsub(' ','',concept),label,sep=''),
#          table=substr(variable,1,6))
# 
# write_csv(vars,'variables.csv')

# stopifnot(1==0)

# ^^^ the stopifnot is so that when we run this section
# we can stop and look for variables
# before doing the really time-consuming stuff below

### 2. get list of variables we want ###
# variables.csv contains list of variables
# as discussed with CCS stakeholders

vars <- read_csv('variables.csv')
v <- vars$variable

### 3. get coordinates of potential locations ###
# again via discussion with CCS

locs <- read_csv('locations.txt')

### 4. get shortage data (from childcare map) ###
# see get_childcare_data.R for how to create shortage.csv

short <- read_csv("shortage.csv",
                  col_names=c('GEOID','shortage'),col_types='c') %>%
  filter(nchar(GEOID)==12) %>%
  mutate(GEOID=substr(GEOID,1,11))
short$shortage=as.numeric(short$shortage)

### 5. get block-group-level families data (from childcare map) ###
# see get_childcare_data.R

fams <- read_csv("families.csv",
                 col_names=c('GEOID','state','fips','fips2','Nfamilies')) %>%
  filter(nchar(GEOID)==12) %>%
  mutate(GEOID=substr(GEOID,1,11))
fams$Nfamilies <- as.numeric(fams$Nfamilies)


### 6. get ACS data ###
# uses Census API key
# get in touch with Amelia if this section doesn't play nice.

acs <- get_acs(geography = 'tract',
                 state = "PA",
                 county="Philadelphia",
                 variables = v,
                 output="wide") %>%
  select(GEOID,ends_with("E")) %>%
  left_join(.,short) %>%
  left_join(.,fams)


### 6. make new variables ###
acs <- rename(acs,
              pop_total=B01001_001E,
              pop_under5=B01001_003E+B01001_027E,
              pct_under5=pop_under5/pop_total,
              households_total=B11016_001E,
              family_households=B11016_002E,
              nonfamily_households=B11016_009E,
              families_total=B17026_001E,
              families_under0.5xpoverty=B17026_002E,
              families_0.5to0.74xpoverty=B17026_003E,
              families_0.75to0.99xpoverty=B17026_004E,
              families_1to1.24xpoverty=B17026_005E,
              families_1.25to1.49xpoverty=B17026_006E,
              families_1.5to1.74xpoverty=B17026_007E,
              families_1.75to1.84xpoverty=B17026_008E,
              families_1.85to1.99xpoverty=B17026_009E,
              families_2to2.99xpoverty=B17026_010E,
              families_3to3.99xpoverty=B17026_011E,
              families_4to4.99xpoverty=B17026_012E,
              families_5andoverxpoverty=B17026_013E,
              median_wage=B20004_001E,
              educ_BA=B15003_022E,
              educ_masters=B15003_023E,
              educ_profesh=B15003_024E,
              educ_phd=B15003_025E) %>%
  mutate(pct_under5=pop_under5/pop_total,
         persons_per_household=pop_total/households_total,
         pct_family_households=family_households/households_total,
         pct_families_inpoverty=(families_under0.5xpoverty+
                                   families_0.5to0.74xpoverty+
                                   families_0.75to0.99xpoverty)/
           families_total,
         families_1to1.99xpoverty=families_1to1.24xpoverty+
           families_1.25to1.49xpoverty+families_1.5to1.74xpoverty+
           families_1.75to1.84xpoverty+families_1.85to1.99xpoverty,
         families_1to3xpoverty=families_1to1.99xpoverty+
           families_2to2.99xpoverty,
         pct_families_1to3xpoverty=families_1to3xpoverty/families_total,
         families_3xpovertyandup=families_3to3.99xpoverty+families_4to4.99xpoverty+
           families_5andoverxpoverty,
         pct_families_over3xpoverty = families_3xpovertyandup/families_total,
         pct_withBA=(educ_BA+educ_masters+educ_profesh+educ_phd)/pop_total,
         pct_morethanBA=(educ_masters+educ_profesh+educ_phd)/pop_total,
         shortage_perfamily=shortage/families_total,
         shortage_perfamily=ifelse(shortage_perfamily< -40,-40,shortage_perfamily),
         above_median_1to3xpoverty=factor(ifelse(families_1to3xpoverty>
                                                   median(families_1to3xpoverty,
                                                          na.rm=TRUE),1,0)),
         above_median_1to3xpoverty_pct=factor(ifelse(
           pct_families_1to3xpoverty>median(pct_families_1to3xpoverty,na.rm=TRUE),
           1,0))) %>%
  select(GEOID,persons_per_household,starts_with("pop"),starts_with("families"),
         starts_with("pct"),shortage,shortage_perfamily,starts_with("above"),
         median_wage) %>%
  filter(pop_total>10,
         families_total>10,
         persons_per_household<=5,
         pct_family_households>=0.10)

tracts <- tracts("PA","Philadelphia") %>%
  select(-NAME)

tracts <- right_join(tracts,acs) %>%
  st_crop(.,xmin=-75.26,xmax=-75.17,ymin=39.92,ymax=39.98)

# bgs <- block_groups("PA","Philadelphia") %>%
#   right_join(.,acs) %>%
#   st_crop(.,xmin=-75.26,xmax=-75.17,ymin=39.92,ymax=39.98)

phlroads <- roads('PA',"Philadelphia") %>%
  filter(FULLNAME %in% c("Market St","Spruce St","Lancaster Ave",
                         "Woodland Ave","Grays Ferry Ave","Baltimore Ave",
                         "S 60th St","N 60th St","S 52nd St","N 52nd St",
                         "S 46th St","N 46th St","S 40th St","N 40th St",
                         "S 34th St","N 34th St"),
         !LINEARID %in% c(1105330038676,1105330038661)) %>%
  st_crop(.,xmin=-75.26,xmax=-75.17,ymin=39.92,ymax=39.98)

make_tract_map <- function(varname){
  filename <- paste(varname,'.pdf',sep='')
  ggplot(data=tracts) + 
    geom_sf(aes_string(fill=varname,
                       color='above_median_1to3xpoverty'),
            size=0.3,na.rm=TRUE) +
    scale_fill_gradient2(low='gold',high='purple4',
                         midpoint=median(acs[[varname]],na.rm=TRUE)) +
    geom_sf(data=phlroads,size=0.1)+
    geom_sf_text(data=phlroads,aes(label=FULLNAME),size=3,angle=-7)+
    geom_point(data=locs,aes(x=longitude,y=latitude),size=3)
  ggsave(filename,width=12,height=12)
}

make_bg_map <- function(varname){
  filename <- paste('~/Dropbox/NonWorkProjects/CCS/CCS-needs-assessment/',varname,'-bg.pdf',sep='')
  ggplot(data=bgs) + 
    geom_sf(aes_string(fill=varname),size=0.1,na.rm=TRUE) +
    scale_fill_gradient2(low='gold',high='purple4',
                         midpoint=ifelse(v=='median_wage',40000,
                                         median(acs[[varname]],na.rm=TRUE))) +
    geom_sf(data=phlroads,size=0.1)+
    geom_sf_text(data=phlroads,aes(label=FULLNAME),size=3,angle=-7)+
    geom_point(data=locs,aes(x=longitude,y=latitude),size=3)
  ggsave(filename,width=12,height=12)
}

# for (v in setdiff(colnames(acs),'GEOID')) {
#   make_tract_map(v)
# }

for (v in setdiff(colnames(acs),c('GEOID','above_median_1to3xpoverty','above_median_1to3xpoverty_pct'))) {
  if(!is.na(median(acs[[v]]))){
    make_tract_map(v)
  }
  else next
}

  

