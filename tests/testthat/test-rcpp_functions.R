
context('rcpp functions')

  
testthat::test_that("the 'convertor_long_cut' function (which uses internally the 'convertor_long_cut_loop' Rcpp function) returns the correct columns and is data.frame of which the column-values sum to greater than 0", {
  
  offs = 500000
  colnams = c("time.seq", "MAL", "CHI", "OCH", "FEM")
  
  long_file = convertor_long_cut(raw_file, onset = 1, offset = offs, cut = 1)
  
  testthat::expect_true( all(colnames(long_file) %in% colnams) & sum(colSums(long_file[, 2:5])) > 0 & inherits(long_file, 'data.frame') )
})


testthat::test_that("the 'LENA_overlap_loop' Rcpp function returns a list of length 2 and the sublists sum to values greater than 0", {
  
  dat_overl_test = LENA_overlap_loop(segment_onset_vec = raw_file$segment_onset, 
                                     segment_offset_vec = raw_file$segment_offset,
                                     speaker_type_vec = raw_file$speaker_type)
  
  lst_nams = c("LENA_OL_segment_onset", "LENA_OL_segment_offset")
  
  testthat::expect_true( all(names(dat_overl_test) %in% lst_nams) & all(unlist(lapply(dat_overl_test, function(x) sum(x) > 0))) & inherits(dat_overl_test, 'list') )
})