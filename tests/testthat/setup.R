

# sample of data used in the Rcpp tests

path_raw_file = file.path(getwd(), 'raw_file.csv')
raw_file = data.table::fread(path_raw_file, stringsAsFactors = F, header = T)
