#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

duse_raw = read.csv('/Users/mcmlxviii/Documents/NYCDS Academy/RSTUDIO/R_PROJ/VSRR_Provisional_Drug_Overdose_Death_Counts.csv', stringsAsFactors = FALSE)
duse <- duse_raw %>% dplyr::select(c(c(1:6,9))) %>% rename_with(tolower)
# FILTER ONLY ON DECEMBER #'S
duse_yr <- duse %>% filter(month=='December' & state!='US')
# FILTER DEATHS_PER_STATE AND OD_DEATHS_PER_STATE
nod_tot <- duse_yr %>% filter(grepl('deaths', indicator, ignore.case=TRUE, perl=TRUE))
# ADD A MUTATED COLUMN TO SHOW THE PERCENTAGE DEATH OF ALL DEATHS
nod_tot <- nod_tot %>% mutate('pct_od' = 0)
# ADD A TYPE COLUMN FOR THE PAIR OF ROWS PER YEAR
# THIS WILL HELP WITH ROW SEARCH FUNCTION
nod_tot <- nod_tot %>% mutate(typ = ifelse((indicator=='Number of Deaths'), 0,1))


searchIndexN <- function(st, yr, typ){
  idx <- which(nod_tot$state==st & nod_tot$year==yr & nod_tot$typ==typ)
  denom <- nod_tot[idx, 'data.value']
  
  idx <- which(nod_tot$state==st & nod_tot$year==yr & nod_tot$typ==1)
  numer <- nod_tot[idx, 'data.value']
  
  return((numer / denom) * 100)
}


# PCT_OD = (data.value/nod_tot[ serachIndN(s,y,0), 'd.v' ]) * 100
nod_tot <- nod_tot %>% rowwise %>% mutate(pct_od =
                                            ifelse(grepl('Overdose', indicator, perl=TRUE), searchIndexN(state,year,0), 0))

# USE ORDER TO GET INDEXES OF PERCENTAGE
od_order <- order(nod_tot$pct_od, decreasing = TRUE)
temp <- data.frame( nod_tot$pct_od[od_order], nod_tot$state[od_order], nod_tot$year[od_order])
names(temp) <- c('pct', 'state', 'year')

# GET OVERDOSE PCTGE ROWS
state_pct_od <- temp %>% filter(pct != 0)

#RETURN STATE FACTOR COLUMNS TO CHAR
states <- unique(nod_tot %>% dplyr::select(state,state.name) %>% arrange(state) %>% 
                   mutate(state=as.character(state)) %>% mutate(state.name=as.character(state.name)))

# REGION DEFINITION VECTORS
Northeast <- c('CT','MA','ME','NH','NJ','NY','PA','RI','VT','YC')
Midwest <- c('IA','IL','IN','KS','MI','MN','MO','ND','NE','OH','SD','WI')
SouthAtlantic <- c('DC','DE','FL','GA','MD','NC','SC','VA','WV')
South <- c('AL','AR','KY','LA','MS','OK','TN','TX')
West <- c('AK','AZ','CA','CO','HI','ID','MT','NM','NV','OR','UT','WA','WY')

# ADD/FILL REGION COLUMN
state_pct_od <- state_pct_od %>% mutate(region  =  case_when(
  state %in% Northeast ~ 'Northeast',
  state %in% Midwest ~ 'Midwest',
  state %in% SouthAtlantic ~ 'SouthAtlantic',
  state %in% West ~ 'West',
  state %in% South ~ 'South',
  TRUE ~ as.character('Unavailable')
))

# ********* #
