#CONSTANTS
YEAR = "YEAR"
ID = "ID"
BEN_CONTR = "BEN_CONTR"
Q26 = c("26.1","26.2","26.3","26.4","26.5","26.6","26.7","26.8","26.9","26.10","26.11",
        "26.12","26.13","26.14","26.15","26.16","26.17","26.18","26.19","26.20","26.21",
        "26.22","26.23","26.24","26.25","26.26","26.27")
Q38 = "38"

null_data <- list()
null_data_col <- 1

CheckLibraries <- function () {
    if (any(grepl("xlsx",installed.packages())) == F || any(grepl("assertthat",installed.packages())) == F){
        print('Please install xlsx library install.packages("xlsx") || install.packages("assertthat") ')
        stop()
    }

    library("xlsx")
    library("assertthat")
}

CreateDataFrame <- function(dataframe, start, end, head){
    return_list <- list()
    row_counter <- 1

    if ((end-start)+1 == length(head)){
        for (cols in start:end){
            datalist <- c(head[row_counter])
            null_values <- 0
            for (data in dataframe[,cols]){
                if (is.numeric(data)){
                    if (is.na(data)){
                        datalist <- c(datalist, data)
                        null_values = null_values + 1
                    }
                    else{
                        if (data == 99){
                            null_values = null_values + 1
                        }
                        datalist <- c(datalist, data)
                    }
                }
            }

            null_data[[null_data_col]] <<- c(head[row_counter], null_values, (100*null_values)/length(dataframe[,cols]))
            datalist <- c(datalist, (c("Null Info", null_values, (100*null_values)/length(dataframe[,cols]))))
            null_data_col <<- null_data_col + 1

            return_list[[row_counter]] <- datalist
            row_counter <- row_counter + 1
        }
    }
    return(return_list)
}

Main <- function (){
    CheckLibraries()

    #BBDD_PATH <- paste(getwd(),"/BBDD_Trabjo Final_EPG.xlsx", sep="")
    BBDD_NAME <- "BBDD_Trabjo Final_EPG.xlsx"
    imported_data <- read.xlsx(BBDD_NAME, sheetIndex = 1)

    general_list <- list()
    Qst26 <- list()
    extra_data <- list()

    general_list[[1]] <- CreateDataFrame(imported_data, 1, 2, c(YEAR, ID))
    general_list[[2]] <- CreateDataFrame(imported_data, 11, 11, c(BEN_CONTR))
    general_list[[3]] <- CreateDataFrame(imported_data, 180, 180, c(Q38))

    Qst26[[1]] <- CreateDataFrame(imported_data, 115, 141, c(Q26))

    extra_data[[1]] <- CreateDataFrame(imported_data, 5, 5, c("REGION"))
    extra_data[[2]] <- CreateDataFrame(imported_data, 10, 10, c("ZONA"))
    extra_data[[3]] <- CreateDataFrame(imported_data, 12, 15, c("EDAD", "SEXO", "PRO_FOMENTO", "PRO_AYUDA"))
    extra_data[[4]] <- CreateDataFrame(imported_data, 92, 96, c("T_OPERACION", "T_PROPIO", "T_ARRENDADO", "T_PROD", "T_RIEGO"))
    extra_data[[5]] <- CreateDataFrame(imported_data, 97, 100, c("BEN_CURSO", "BEN_NIVEL", "CONY_CURSO", "CONY_NIVEL"))

    #Main data
    write.xlsx(general_list, file="myworkbook.xlsx", 
                sheetName="main_data", row.names=FALSE, col.names = FALSE)
    #Question 26 data
    write.xlsx(Qst26, file="myworkbook.xlsx", 
                sheetName="Q26", append=TRUE, row.names=FALSE, col.names = FALSE)
    #Other data
    write.xlsx(extra_data, file="myworkbook.xlsx", 
                sheetName="extra_data", append=TRUE, row.names=FALSE, col.names = FALSE)
    #Info about null data
    write.xlsx(null_data, file="myworkbook.xlsx", 
                sheetName="null_data", append=TRUE, 
                row.names=FALSE, col.names = FALSE)
}

Main()