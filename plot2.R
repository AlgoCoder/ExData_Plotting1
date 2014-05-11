global_active_power_line <- function(file = stop(" 'file' must be specified"), toDate = "2007-02-01", fromDate = "2007-02-02") {
  
  err.file.handler <- function(...) stop("file not accessible, check path / permissions", call. = F)  
  err.date.handler <- function(expr) stop(paste(expr, " not a valid 'date' "), call. = F)
  err.pkg.handler  <- function(expr) stop(paste("Failed to load package : ", expr, " . Install package or check lib.loc"), call. = F)
  
  toDate <- tryCatch(as.Date(toDate), error = function(e) err.date.handler(toDate))
  fromDate <- tryCatch(as.Date(fromDate), error = function(e) err.date.handler(fromDate))  
  
  if(file.access(file, 4) == 0) {
      
      pkg.depend <- "sqldf"
      pkg.load.result <- FALSE
      suppressWarnings( {
          pkg.load.result <- require(pkg.depend, character.only = T, quietly = T)
      })
      if(!pkg.load.result)
          err.pkg.handler(pkg.depend)
      
      initial.df <- read.csv(file, nrows = 10, sep = ";")
      classes <- sapply(initial.df, class)
            
      extract.df <- read.csv.sql(file, sql = "select * from file where (Date == '1/2/2007') or (Date == '2/2/2007') ", colClasses = classes, sep = ";", eol = "\n")
            
      extract.df[, "Date"] <- paste(extract.df[, "Date"], extract.df[, "Time"], sep = " ")
      extract.df[, "Date"] <- as.POSIXct(extract.df[, "Date"], format = "%d/%m/%Y %H:%M:%S")
      extract.df[, "Time"] <- NULL
      
      png(file = "plot2.png")
      with(extract.df, plot(x = Date, y = Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xlab = ""))
      dev.off()
      
      on.exit(closeAllConnections())
      
  }
  else {
      err.file.handler(file, toDate, fromDate)
  }
  
} 