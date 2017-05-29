###############################################################################
###############################################################################
###############################################################################

## instaluji a loaduji balíčky ------------------------------------------------

invisible(
    lapply(c(
            "xtable",
            "openxlsx",
            "rpart",
            "rpart.plot",
            "RColorBrewer",
            "rattle"
        ),
        function(package){
            
            if(!(package %in% rownames(installed.packages()))){
    
                install.packages(
                    package,
                    dependencies = TRUE,
                    repos = "http://cran.us.r-project.org"
                )
        
            }
  
            library(package, character.only = TRUE)
            
        }
    )
)


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji handling se zipováním v R ----------------------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip") 


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(!"script.R" %in% dir()){
    setwd(choose.dir())
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím posložky pracovní složky ------------------------------------------

setwd(mother_working_directory)

for(my_subdirectory in c("vstupy", "vystupy")){
    
    if(!file.exists(my_subdirectory)){

        dir.create(file.path(
        
            mother_working_directory, my_subdirectory
            
        ))
        
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################

## loaduji data ---------------------------------------------------------------

setwd(paste(mother_working_directory, "vstupy", sep = "/"))

my_data <- read.csv(
    
    file = "Adamek06.csv",
    header = TRUE,
    sep = ";",
    check.names = FALSE,
    colClasses = "character"
        
)

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## preprocessing --------------------------------------------------------------

#### vytvářím vektor indexů těch proměnných, které nejsou příliš řídké
#### (nemají tolik chybějícíh hodnot) -----------------------------------------

less_sparse_variables <- c(
    
    1:6,                               ## identifikátory, osobní informace
    8, 10, 17, 18, 19, 20, 25, 27,     ## skupina proměnných o otci
    32, 34, 41, 42, 43, 44, 49, 51,    ## skupina proměnných o matce
    55,                                ## lékař
    56, 57, 58,                        ## osobní informace
    59, 60, 61,                        ## zátěž
    62,                                ## kouření
    68, 69, 70,                        ## alkohol
    71,                                ## alergie
    74,                                ## ICHS
    90,                                ## ICHPT
    97,                                ## CMP
    103,                               ## diabete mellitus
    109,                               ## hypertenze
    114,                               ## hyperlipoproteinémie
    119,                               ## aneurysma
    123,                               ## komorbidity
    128:135,                           ## symptomatologie
    136,                               ## léčen dietou?
    141,                               ## léčen farmakologicky?
    146:158,                           ## fyzikální vyšetření, status praesens
    165:170,                           ## laboratorní nález
    171, 174:182, 191, 194,            ## EKG
    197,                               ## ambulance, ve které vyšetřen
    198,                               ## datum vyšetření
    199, 200                           ## věk úmrtí matky a otce
    
)


meaningful_variables <- c(
    
    4, 5,                              ## identifikátory, osobní informace
    10,                                ## skupina proměnných o otci
    34,                                ## skupina proměnných o matce
    56, 58,                            ## osobní informace
    59, 60,                            ## zátěž
    62,                                ## kouření
    68, 69, 70,                        ## alkohol
    74,                                ## ICHS
    90,                                ## ICHPT
    97,                                ## CMP
    103,                               ## diabete mellitus
    109,                               ## hypertenze
    114,                               ## hyperlipoproteinémie
    119,                               ## aneurysma
    123,                               ## komorbidity
    128:135,                           ## symptomatologie
    136,                               ## léčen dietou?
    141,                               ## léčen farmakologicky?
    146:158,                           ## fyzikální vyšetření, status praesens
    165:170,                           ## laboratorní nález
    171, 174:182, 191, 194#,            ## EKG
    #199, 200                           ## věk úmrtí matky a otce
    
)


response_variables <- c(

    "ICHS",
    "ICHPT",
    "CMP",
    "DM",
    "HT",
    "HLP",
    "AneurysmataAorty",
    "JineChoroby",
    "Dusnost",
    "BolestHrud",
    "Palpitace",
    "Otoky",
    "Synkopa",
    "Kasel",
    "Hemoptyza",
    "Klaudikace",
    "LecDietou",
    "LecLeky"
    
)


## ----------------------------------------------------------------------------

#### převádím proměnné na numerické, či na kategorické ------------------------

for(i in 1:dim(my_data)[2]){
    
    if(
        suppressWarnings(
            all(
                !is.na(
                    as.numeric(
                        gsub(
                            ",",
                            ".",
                            as.character(
                                my_data[!is.na(my_data[, i]), i]
                            )
                        )
                    )
                )
            )
        )
    ){
        
        my_data[, i] <- suppressWarnings(
            as.numeric(
                gsub(
                    ",",
                    ".",
                    as.character(my_data[, i])
                )
            )
        )
        
    }else{
        
        my_data[which(my_data[, i] == "yes"), i] <- "ano"
        my_data[which(my_data[, i] == "no"), i] <- "ne"
        
        my_data[, i] <- as.factor(my_data[, i])        
        
    }
    
}


## ----------------------------------------------------------------------------

#### první dvě proměnné jsou kategorické (identifikátory) ---------------------

for(i in 1:2){
    
    my_data[, i] <- as.factor(as.character(my_data[, i]))
    
}


#### třetí a 198. proměnná jsou datumy ----------------------------------------

for(i in c(3, 198)){
    
    my_data[, i] <- as.Date(as.character(my_data[, i]), "%d.%m.%Y")
    
}


#### některé proměnné jsou numerické, i přesto že obsahují volný text;
#### převádím je i za cenu ztráty některých hodnot ----------------------------

for(i in c(
    
    68, 69, 70,
    146:149,
    151:157,
    165:170,
    173:181,
    199:200
    
)){
    
    my_data[, i] <- suppressWarnings(as.numeric(as.character(my_data[, i])))
    
}


## ----------------------------------------------------------------------------

#### handling s chybějícími hodnotami -----------------------------------------
#### v případě kategorické proměnné nahrazuji chybějící hodnotu modusovou
#### hodnotou, v případě numerické proměnné průměrnou hodnotou ----------------

my_data_wo_NA <- my_data

for(i in 1:dim(my_data)[2]){

    if(class(my_data[, i]) == "numeric"){
        
        if(any(is.na(my_data[, i]))){
        
            my_data_wo_NA[which(is.na(my_data_wo_NA[, i])), i] <- mean(
                my_data[, i],
                na.rm = TRUE
            )
            
        }
    
    }
        
    if(class(my_data[, i]) == "factor"){
        
        if(any(my_data[, i] == "")){            
            
            my_data_wo_NA[, i] <- as.character(my_data_wo_NA[, i])
            
            if(length(my_data_wo_NA[, i][my_data_wo_NA[, i] != ""]) > 0){
                
                my_data_wo_NA[which(my_data_wo_NA[, i] == ""), i] <- max(
                    my_data_wo_NA[, i][my_data_wo_NA[, i] != ""]
                )
                
            }
                        
            my_data_wo_NA[, i] <- as.factor(my_data_wo_NA[, i])
            
        }
    
    }
    
   
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## začínám s analýzami --------------------------------------------------------

###############################################################################

#### rozhodovací stromy -------------------------------------------------------

###############################################################################

## helper funkce --------------------------------------------------------------

getMyAccuracy <- function(my_table){
    
    # '''
    # vrací přesnost pro konfuzní matici "my_table"
    # '''
    
    return(sum(diag(my_table)) / sum(my_table))
    
}


## ----------------------------------------------------------------------------

###############################################################################

## zkouším rozhodovací stromy -------------------------------------------------

#### nejdříve rozděluji data na trénovací a testovací množinu -----------------

#### do trénovací množiny zahrnu 70 % dat -------------------------------------

train_set_portion <- 0.7

set.seed(2017)


#### vytvářím množinu indexů pozorování, která budou zahrnuta do trénovací
#### množiny ------------------------------------------------------------------

train_set_indices <- sample(
    c(1:dim(my_data)[1]),
    floor(dim(my_data)[1] * train_set_portion),
    replace = FALSE
)


#### vytvářím trénovací a testovací množinu -----------------------------------

train_set <- my_data_wo_NA[train_set_indices, ]
test_set <- my_data_wo_NA[setdiff(c(1:dim(my_data)[1]), train_set_indices), ]


#### učím stromy --------------------------------------------------------------

size_table <- setNames(
    c(0.8, 1, 1, 1, 0.6, 0.4, 1, 0.4, 0.8, 1, 0.6, 1, 1, 1, 1, 1, 0.8, 0.8),
    response_variables    
)


for(my_variable in response_variables){
    
    ## formuluji závislost proměnné na ostatních ------------------------------
    
    my_formula <- paste(
    
        my_variable,
        " ~ ",
        paste(setdiff(
            colnames(train_set[, response_variables]),
            my_variable
        ), collapse = " + "),
        sep = ""
        
    )
    
    
    ## nechám vyrůst daný strom -----------------------------------------------
    
    eval(
        parse(
            text = paste(
                "my_tree",
                " <- ",
                "rpart(",
                "formula = ",
                my_formula,
                ", ",
                "data = train_set[, meaningful_variables]",
                ")",
                sep = ""
            )
        )
    )
    
    
    ## ukládám strom do souboru -----------------------------------------------
        
    setwd(paste(mother_working_directory, "vystupy", sep = "/"))
    
    cairo_ps(
        file = paste(my_variable, "_tree.eps", sep = ""),
        width = 8 * size_table[my_variable],
        height = 5 * size_table[my_variable],
        pointsize = 18
    )
    
    par(mar = c(0, 0, 0, 0))

    prp(
        my_tree,
        varlen = 20,
        faclen = 13,
        yes.text = "ano",
        no.text = "ne"
    )

    dev.off()

    setwd(mother_working_directory)
    
    
    ## konfuzní matice a přesnost ---------------------------------------------
    
    predict(object = my_tree, newdata = test_set, type = "class")

    my_table <- table(
        test_set[, my_variable],
        predict(object = my_tree, newdata = test_set, type = "class"),
        dnn = list("skutečné hodnoty", "predikované hodnoty")
    )    
    
    print(
        "###################################################################"
    )
    
    print(
        paste("Proměnná ", my_variable, sep = "")
    )
    
    print(
        xtable(
            my_table,
            align = rep("", ncol(my_table) + 1),
            digits = 3
        ),
        floating = FALSE,
        tabular.environment = "tabular",
        hline.after = NULL,
        include.rownames = TRUE,
        include.colnames = TRUE
    )
    
    print(getMyAccuracy(my_table))
    
        
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





