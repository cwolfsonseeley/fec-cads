bucket_url <- function(year) {
    basepage_url <- paste("https://www.fec.gov/files/bulk-downloads/index.html?prefix=bulk-downloads/", year, sep = "")
    basepage <- readLines(basepage_url)
    bucketurl <- basepage[
        stringr::str_detect(basepage, "BUCKET_URL")
    ]
    if (length(bucketurl) != 1) stop("Can't find bucket URL")
    stringr::str_match(bucketurl, "BUCKET_URL\\s*=\\s*\\'(.+)\\'")[,2]
}

get_fec <- function(year, location = "./data/fec",
                    files = c("cm", "cn", "ccl", "oth", "pas2", "indiv"), 
                    ...) {
    destdir <- paste(location, year, sep = "/")
    if (!dir.exists(destdir)) dir.create(destdir, recursive = TRUE)
    filenames <- paste(files, stringr::str_sub(year, 3, 4), ".zip", sep="")
    destfiles <- paste(destdir, filenames, sep = "/")
    #srcfiles <- paste("ftp://ftp.fec.gov/FEC", year, filenames, sep = "/")
    srcfiles <- paste(bucket_url(year), "bulk-downloads", year, filenames, sep = "/")
    Map(download.file, srcfiles, destfiles)
}

unzipper <- function(year, filename, location = "./data/fec", 
                     headers = "./data/fec/headers",
                     dd = "./data/fec/dd") {
    # unzip the file to a temporary directory,
    # and save the name of the text file to "uncompressed"
    zipfile <- paste(filename, stringr::str_sub(year, 3, 4), ".zip", sep = "")
    zipfile <- paste(location, year, zipfile, sep = "/")
    ziplist <- unzip(zipfile, list = TRUE)
    uncompressed <- if (nrow(ziplist) == 1L) {
        ziplist[[1]] } else {
            ziplist[[1]][which.max(ziplist$Length)]
        }
    unzipped <- unzip(zipfile, uncompressed, exdir = tempdir())
    
    # make sure to clean up
    on.exit(unlink(unzipped))
    
    # use data-dictionary to inform column types
    # nothing fancy, all characters unless explicitly known to be number
    # will have to transform dates manually
    dd_filename <- paste("dd_", filename, ".rds", sep = "")
    dict <- readRDS(paste(dd, dd_filename, sep = "/"))
    #coltypes <- paste(dict$coltype, collapse = "")
    coltypes <- paste(rep("c", nrow(dict)), collapse = "")
    
    df <- readr::read_delim(unzipped, delim = "|", col_names = FALSE, 
                            col_types = coltypes, escape_backslash = FALSE)
    header_file <- paste(headers, "/", filename, "_header_file.csv", sep = "")
    header <- readLines(header_file, n = 1)
    header <- strsplit(header, ",")[[1]]
    names(df) <- tolower(header)
    attr(df, "dd") <- dict
    attr(df, "column") <- function(colname) {
        lookup <- attr(df, "dd")
        lookup[tolower(lookup$column) == colname,]
    }
    df
}

coldef <- function(df, colname) {
    attr(df, "column")(colname)
}

committees <- function(year, ...) unzipper(year, "cm", ...)
individuals <- function(year, ...) unzipper(year, "indiv", ...)

chunked_unzipper <- function(year, filename, location = "./data/fec", 
                             headers = "./data/fec/headers",
                             dd = "./data/fec/dd",
                             con = getcdw::connect("URELUAT_DEVEL"),
                             chunk_size = 1000000) {
    # unzip the file to a temporary directory,
    # and save the name of the text file to "uncompressed"
    zipfile <- paste(filename, stringr::str_sub(year, 3, 4), ".zip", sep = "")
    zipfile <- paste(location, year, zipfile, sep = "/")
    ziplist <- unzip(zipfile, list = TRUE)
    uncompressed <- if (nrow(ziplist) == 1L) {
        ziplist[[1]] } else {
            ziplist[[1]][which.max(ziplist$Length)]
        }
    unzipped <- unzip(zipfile, uncompressed, exdir = tempdir())
    
    # make sure to clean up
    on.exit(unlink(unzipped))
    
    # use data-dictionary to inform column types
    # nothing fancy, all characters unless explicitly known to be number
    # will have to transform dates manually
    dd_filename <- paste("dd_", filename, ".rds", sep = "")
    dict <- readRDS(paste(dd, dd_filename, sep = "/"))
    coltypes <- paste(rep("c", nrow(dict)), collapse = "")

    header_file <- paste(headers, "/", filename, "_header_file.csv", sep = "")
    header <- readLines(header_file, n = 1)
    header <- strsplit(header, ",")[[1]]
    header <- tolower(header)
    
    # now read in chunks and load them into the cdw
    stage_chunk <- function(chunk, pos) {
        names(chunk) <- header
        res <- ROracle::dbWriteTable(
            con, "FEC_STAGE", 
            chunk %>% 
                filter(entity_tp %in% c("CAN", "IND")) %>% 
                select(sub_id, name, city, state, zip_code, employer, occupation), 
            schema = 'RDATA',
            overwrite = FALSE, append = TRUE)
        ROracle::dbCommit(con)
    }
    
    readr::read_delim_chunked(
        file = unzipped, 
        delim = "|",
        col_names = FALSE,
        col_types = coltypes,
        escape_backslash = FALSE,
        callback = stage_chunk,
        chunk_size = chunk_size)
}

ind_by_subid <- function(year, location = "./data/fec", 
                         headers = "./data/fec/headers",
                         dd = "./data/fec/dd",
                         sub_ids, 
                         chunk_size = 1000000) {
    
    filename <- "indiv"
    # unzip the file to a temporary directory,
    # and save the name of the text file to "uncompressed"
    zipfile <- paste(filename, stringr::str_sub(year, 3, 4), ".zip", sep = "")
    zipfile <- paste(location, year, zipfile, sep = "/")
    ziplist <- unzip(zipfile, list = TRUE)
    uncompressed <- if (nrow(ziplist) == 1L) {
        ziplist[[1]] } else {
            ziplist[[1]][which.max(ziplist$Length)]
        }
    unzipped <- unzip(zipfile, uncompressed, exdir = tempdir())
    
    # make sure to clean up
    on.exit(unlink(unzipped))
    
    dd_filename <- paste("dd_", filename, ".rds", sep = "")
    dict <- readRDS(paste(dd, dd_filename, sep = "/"))
    coltypes <- paste(rep("c", nrow(dict)), collapse = "")
    
    header_file <- paste(headers, "/", filename, "_header_file.csv", sep = "")
    header <- readLines(header_file, n = 1)
    header <- strsplit(header, ",")[[1]]
    header <- tolower(header)
    
    # now read in chunks and keep just the important columns
    # and only the rows that have matches
    import_matching_rows <- function(chunk, pos) {
        names(chunk) <- header
        chunk %>%
            filter(sub_id %in% sub_ids) %>% 
            select(sub_id, cmte_id, image_num,
                   transaction_tp, transaction_dt, transaction_amt)
    }
    
    readr::read_delim_chunked(
        file = unzipped, 
        delim = "|",
        col_names = FALSE,
        col_types = coltypes,
        escape_backslash = FALSE,
        callback = DataFrameCallback$new(import_matching_rows),
        chunk_size = 1000000)
}
