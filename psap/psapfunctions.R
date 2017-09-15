###############################################################################
###############################################################################

# Contains 3 PSAP functions

# Function 1: calculates PSAP values at the individual level
# Function 2: calculates PSAP summary table
# Function 3: generates PSAP plot

# Authors: Bas Baccarne & Aron-Levi Herregodts

###############################################################################
############################# MAIN PSAP function ##############################
###############################################################################

psap <- function(psap.df){
        
        # agrument psap.df = data.frame with three comuns (psap1, psap2, psap3)
        
        # check argument requirements
        
        if(is.data.frame(psap.df)==FALSE) {
                print("argument needs to be a data.frame")}
        if((ncol(psap.df)==3)==FALSE) {
                print("argument needs to have 3 columns (PSAP1, PSAP2 & PSAP3)")}
        if(any((lapply(psap.df,class))!="integer")==TRUE) {
                print("values in columns need to be numeric")}
        
        stopifnot(is.data.frame(psap.df)==TRUE)
        stopifnot((ncol(psap.df)==3)==TRUE)
        stopifnot(any((lapply(psap.df,class))!="integer")==FALSE)
        
        # initialize data
        
        data <- psap.df
        names(data) <- c("PSAP1", "PSAP2", "PSAP3")

        # calculate PSAP values at the individual level
        
        for(i in 1: nrow(data)) {

                if(any(is.na(data[i,]))){data$PSAP[i] <- NA}
                
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==1 & data$PSAP3[i]==1){data$PSAP[i] <- 1}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==1 & data$PSAP3[i]==2){data$PSAP[i] <- 1}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==1 & data$PSAP3[i]==3){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==1 & data$PSAP3[i]==4){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==1 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==2 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==2 & data$PSAP3[i]==2){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==2 & data$PSAP3[i]==3){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==2 & data$PSAP3[i]==4){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==2 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==3 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==3 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==3 & data$PSAP3[i]==3){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==3 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==3 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==4 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==4 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==4 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==4 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==4 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==5 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==5 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==5 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==5 & data$PSAP3[i]==4){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==1 & data$PSAP2[i]==5 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==1 & data$PSAP3[i]==1){data$PSAP[i] <- 1}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==1 & data$PSAP3[i]==2){data$PSAP[i] <- 1}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==1 & data$PSAP3[i]==3){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==1 & data$PSAP3[i]==4){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==1 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==2 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==2 & data$PSAP3[i]==2){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==2 & data$PSAP3[i]==3){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==2 & data$PSAP3[i]==4){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==2 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==3 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==3 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==3 & data$PSAP3[i]==3){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==3 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==3 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==4 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==4 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==4 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==4 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==4 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==5 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==5 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==5 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==5 & data$PSAP3[i]==4){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==2 & data$PSAP2[i]==5 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==1 & data$PSAP3[i]==1){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==1 & data$PSAP3[i]==2){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==1 & data$PSAP3[i]==3){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==1 & data$PSAP3[i]==4){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==1 & data$PSAP3[i]==5){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==2 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==2 & data$PSAP3[i]==2){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==2 & data$PSAP3[i]==3){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==2 & data$PSAP3[i]==4){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==2 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==3 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==3 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==3 & data$PSAP3[i]==3){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==3 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==3 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==4 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==4 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==4 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==4 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==4 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==5 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==5 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==5 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==5 & data$PSAP3[i]==4){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==3 & data$PSAP2[i]==5 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==1 & data$PSAP3[i]==1){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==1 & data$PSAP3[i]==2){data$PSAP[i] <- 2}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==1 & data$PSAP3[i]==3){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==1 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==1 & data$PSAP3[i]==5){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==2 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==2 & data$PSAP3[i]==2){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==2 & data$PSAP3[i]==3){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==2 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==2 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==3 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==3 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==3 & data$PSAP3[i]==3){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==3 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==3 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==4 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==4 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==4 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==4 & data$PSAP3[i]==4){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==4 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==5 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==5 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==5 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==5 & data$PSAP3[i]==4){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==4 & data$PSAP2[i]==5 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==1 & data$PSAP3[i]==1){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==1 & data$PSAP3[i]==2){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==1 & data$PSAP3[i]==3){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==1 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==1 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==2 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==2 & data$PSAP3[i]==2){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==2 & data$PSAP3[i]==3){data$PSAP[i] <- 3}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==2 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==2 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==3 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==3 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==3 & data$PSAP3[i]==3){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==3 & data$PSAP3[i]==4){data$PSAP[i] <- 4}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==3 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==4 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==4 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==4 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==4 & data$PSAP3[i]==4){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==4 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==5 & data$PSAP3[i]==1){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==5 & data$PSAP3[i]==2){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==5 & data$PSAP3[i]==3){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==5 & data$PSAP3[i]==4){data$PSAP[i] <- 0}
                else if(data$PSAP1[i]==5 & data$PSAP2[i]==5 & data$PSAP3[i]==5){data$PSAP[i] <- 5}
        }
        
        # invalid patterns are converted in NAs
        # data$PSAP[data$PSAP==0] <- NA
        
        # output = data.frame with 4 columns (PSAP1, PSAP2, PSAP3 & PSAP)
        data
        
}

###############################################################################
############################# PSAP SUMMARY TABLE ##############################
###############################################################################

psapTable <- function(psap.df){
        
        rawdata <- psap(psap.df)
        rawdata <- rawdata[complete.cases(rawdata), ]
        rawdata$PSAP[rawdata$PSAP==0] <- NA
        
        # functie die het confidence interval berekenent
        CI_percentage = function(k,n){
                # Stap 1: subsamplegrootte (k) en totale steekproefgrootte (n)
                k = k   
                n = n
                # Stap 2: proportie (p) wordt berekend
                p = k/n
                # Stap 3: standard error (SE) & margin of error (E) worden berekend
                library(MASS)
                SE = sqrt(p*(1-p)/n)
                E = qnorm(.975)*SE
                # Stap 4: bewtrouwbaarheidsinterval (CI) wordt berekend
                CI = p + c(-E, E) 
                # Stap 5: print de outcome
                outcome =  c(CI[1], p, CI[2])
                names(outcome) = c("lower bound", "proportion", "upper bound") 
                outcome
        }
        
        # aanmaken van een table met de ruwe berekening voor de grafiek
        total <- nrow(rawdata)
        PSAP_table <- as.data.frame(table(rawdata$PSAP))
        names(PSAP_table) <- c("profiel", "N")
        PSAP_table$profiel <- c("innovators", 
                                "early adopters", 
                                "early majority", 
                                "late majority", 
                                "laggards")
        PSAP_table$profiel <- factor(PSAP_table$profiel, levels = PSAP_table$profiel)
        
        
        for (i in 1:nrow(PSAP_table)){
                PSAP_table$lowerbound[i] <- CI_percentage(PSAP_table$N[i], total)[1]
                PSAP_table$proportion[i] <- CI_percentage(PSAP_table$N[i], total)[2]
                PSAP_table$upperbound[i] <- CI_percentage(PSAP_table$N[i], total)[3]
        }
        
        PSAP_table
}

###############################################################################
################################# PSAP GRAPH ##################################
###############################################################################

psapGraph <- function(psap.df){
        
        library(ggplot2)
        
        PSAP_table <- psapTable(psap.df)
        
        g = ggplot(data=PSAP_table, aes(x=profiel, y=proportion,group=1))
        g = g + geom_ribbon(aes(ymin=lowerbound,
                                ymax=upperbound),
                            fill = "grey70",
                            alpha=0.5)
        g = g + geom_point(aes(
                size = 10,
                alpha=0.8))
        g = g + geom_line()
        g = g + theme(legend.position="none")
        g
        
        # [actiepunt: imec huisstijl]
}