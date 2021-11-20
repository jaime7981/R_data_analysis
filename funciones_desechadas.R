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


create_data_frame_no_null <- function(dataframe, start, end, head) { # nolint
    return_list <- list()
    row_counter <- 1

    if ((end - start) + 1 == length(head)) {
        for (cols in start:end) {
            mean_data <- 0
            calc_mean <- 0

            datalist <- c(head[row_counter])
            null_values <- 0

            for (data in dataframe[, cols]) {
                if (is.numeric(data)) {
                    if (is.na(data)) {
                        null_values <- null_values + 1
                    }
                    else{
                        if (data == 99) {
                            null_values <- null_values + 1
                        }
                        else{
                            calc_mean <- calc_mean + data
                        }
                    }
                }
            }

            mean_data <- calc_mean / (length(dataframe[, cols]) - null_values)

            for (data in dataframe[, cols]) {
                if (is.numeric(data)) {
                    if (is.na(data)) {
                        datalist <- c(datalist, mean_data)
                    }
                    else{
                        if (data == 99 || data < 1) {
                            datalist <- c(datalist, mean_data)
                        }
                        else{
                            datalist <- c(datalist, data)
                        }
                    }
                }
            }

            null_data[[null_data_col]] <<- c(head[row_counter],
                                            null_values,
                                            (100 * null_values) /
                                            length(dataframe[, cols]))
            datalist <- c(datalist, (c("Null Info",
                                     null_values,
                                     (100 * null_values) /
                                     length(dataframe[, cols]))))
            null_data_col <<- null_data_col + 1

            if (data_loss_perc < 35) {
                return_list[[row_counter]] <- datalist
                row_counter <- row_counter + 1
            }
        }
    }
    return(return_list)
}

show_summary <- function(data_list, col_name) {
    for (group in data_list) {
        for (category in group) {
            #item <- as.numeric(category)
            #print(category[1])
            #print(summary(item))
            #print(plyr::count(item))
            for (names in col_name) {
                delete_data(category, names)
            }
            #get_frq_mean(plyr::count(item))
            #print(table(item))
        }
    }
}

delete_data <- function(category, col_name) {
    item <- as.numeric(category)
    if (category[1] == col_name) {
        freq_data <- plyr::count(item)

        counter <- 0
        for (data in freq_data$x) {
            counter <- counter + 1
            if (is.na(data)) {
                #print(freq_data$freq[counter])
            }
        }
    }
}

quialitative_mean <- function(data_list, col_name) {
    for (group in data_list) {
        for (category in group) {
            item <- as.numeric(category)
            #print(category[1])
            #print(plyr::count(item))

            if (category[1] == col_name) {
                for (names in col_name) {
                    freq_data <- plyr::count(item)
                    counter <- 0
                    for (data in freq_data$x) {
                        counter <- counter + 1
                        if (is.na(data)) {
                            #print(freq_data$freq[counter])
                        }
                        else if (data < 1) {
                            item[counter] <- NA
                        }
                        else {
                            item[counter] <- 1
                        }
                    }
                }
            }
            #print(summary(item))
        }
    }
}

delete_quialitative_data <- function(category, col_name) {
    item <- as.numeric(category)
    if (category[1] == col_name) {
        freq_data <- plyr::count(item)

        counter <- 0
        for (data in freq_data$x) {
            counter <- counter + 1
            if (is.na(data)) {
                #print(freq_data$freq[counter])
            }
            else if (data < 1) {
                freq_data$freq[counter] <- NA
            }
            else {
                freq_data$freq[counter] <- 0
            }
        }
    }
}