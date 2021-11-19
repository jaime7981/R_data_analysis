DataInRows <- function (imported_data, start, nmbr_of_rows){
    for (rows in start:nmbr_of_rows){
        row = imported_data[rows,]
        counter = 0
        counter_26 = 1
        datalist = c()

        for (data in row){
            if (counter >= 114 && counter <= 140){
                print(paste("Pregunta 26.", counter_26, ": ", data, sep=""))
                counter_26 = counter_26 + 1
                datalist <- c(datalist, data)
            }
            else if (counter == 0){
                print(paste("Ano:", data))
                datalist <- c(datalist, data)
            }
            else if (counter == 1){
                print(paste("ID:", data))
                datalist <- c(datalist, data)
            }
            else if (counter == 179){
                print(paste("Participacion en org agricolas:", data))
                datalist <- c(datalist, data)
            }
            counter = counter + 1
        }
        general_data <- lappend(general_data, datalist)
    }
    return(general_data)
}

CheckDataType <- function(data) {
    if (is.numeric(data)){
        if (is.null(data)){
            print("Null data")
        }
        else if (is.na(data)){
            print(paste("NA data", data))
        }
        else{
            print(paste("Is numeric", data))
        }
    }
    else if (typeof(data) == "integer"){
        if (data != ""){
            print(paste(typeof(data), data))
        }
        else{
            print("Empty value interpreted as integer")
        }
    }
    else{
        print(paste("Unknoun data type", typeof(data), data))
    }
}


lappend <- function (lst, ...){
    lst <- c(lst, list(...))
    return(lst)
}