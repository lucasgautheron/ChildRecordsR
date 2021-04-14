# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins("openmp")]]
// [[Rcpp::plugins("cpp11")]]
# include <algorithm>

#ifdef _OPENMP
# include <omp.h>
#endif


//' Rcpp function to replace the for-loop inside the 'convertor_long_cut()' function
//'
//' @param time_seq a numeric vector of the time sequence
//' @param segment_onset_vec a numeric vector of onset
//' @param segment_offset_vec a numeric vector of offset
//' @param speaker_type_vec a character vector of speaker type
//' @return a named list of length 2
// [[Rcpp::export]]
Rcpp::DataFrame convertor_long_cut_loop(arma::vec time_seq,
                                        arma::vec segment_onset_vec,
                                        arma::vec segment_offset_vec,
                                        std::vector<std::string> speaker_type_vec) {
  double segment_onset;
  double segment_offset;
  std::string speaker_type;
  std::vector<std::string> vec_speak_typ = {"MAL", "CHI", "OCH", "FEM"};

  arma::vec MAL(time_seq.n_elem, arma::fill::zeros);
  arma::vec CHI(time_seq.n_elem, arma::fill::zeros);
  arma::vec OCH(time_seq.n_elem, arma::fill::zeros);
  arma::vec FEM(time_seq.n_elem, arma::fill::zeros);

  for (unsigned int i = 0; i < segment_onset_vec.n_elem; i++) {

    segment_onset = segment_onset_vec(i);
    segment_offset = segment_offset_vec(i);
    speaker_type = speaker_type_vec[i];

    if (std::find(vec_speak_typ.begin(), vec_speak_typ.end(), speaker_type) != vec_speak_typ.end()) {

      for (unsigned int j = 0; j < time_seq.n_elem; j++) {

        bool bool_inner = (time_seq(j) >= segment_onset) && (time_seq(j) <= segment_offset);

        if (bool_inner) {

          if (speaker_type == "MAL") {
            MAL(j) += 1;
          }
          else if (speaker_type == "CHI") {
            CHI(j) += 1;
          }
          else if (speaker_type == "OCH") {
            OCH(j) += 1;
          }
          else if (speaker_type == "FEM") {
            FEM(j) += 1;
          }
          else {
            Rcpp::stop("Valid methods are: 'MAL', 'CHI', 'OCH' and 'FEM'!");
          }
        }
      }
    }
  }

  Rcpp::List lst_df = Rcpp::List::create( Rcpp::Named("time.seq") = time_seq,
                                          Rcpp::Named("MAL") = MAL,
                                          Rcpp::Named("CHI") = CHI,
                                          Rcpp::Named("OCH") = OCH,
                                          Rcpp::Named("FEM") = FEM);
  Rcpp::DataFrame result(lst_df);
  return result;
}


//' Rcpp function to replace the for-loop inside the 'LENA.overlap()' function
//'
//' @param segment_onset_vec a numeric vector of onset
//' @param segment_offset_vec a numeric vector of offset
//' @param speaker_type_vec a character vector of speaker type
//' @return a named list of length 2
// [[Rcpp::export]]
Rcpp::List LENA_overlap_loop(arma::vec segment_onset_vec,
                             arma::vec segment_offset_vec,
                             std::vector<std::string> speaker_type_vec) {
  double segment_onset;
  double segment_offset;
  std::string speaker_type;

  arma::vec LENA_OL_segment_onset(segment_onset_vec);                // copy the onset and offset vectors
  arma::vec LENA_OL_segment_offset(segment_offset_vec);

  for (unsigned int i = 0; i < segment_onset_vec.n_elem; i++) {

    segment_onset = segment_onset_vec(i);
    segment_offset = segment_offset_vec(i);
    speaker_type = speaker_type_vec[i];

    if (speaker_type != "SPEECH") {

      for (unsigned int j = 0; j < LENA_OL_segment_offset.n_elem; j++) {

        bool l_test = (LENA_OL_segment_offset(j) > segment_onset) && (LENA_OL_segment_offset(j) < segment_offset) && (LENA_OL_segment_onset(j) <= segment_onset);       // overlap left ( find left overlap )

        if (l_test) {
          LENA_OL_segment_offset(j) = segment_onset;             // Change row overlap
          segment_onset = segment_offset_vec(j);                 // change evaluated row   [ !!!!!! in the R code here was a 'max()' function because there was a vector of values whereas I have a single value ]
        }

        bool r_test = (LENA_OL_segment_onset(j) < segment_offset) && (LENA_OL_segment_onset(j) > segment_onset) && (LENA_OL_segment_offset(j) >= segment_offset);        // overlap right find left overlap

        if(r_test) {
          LENA_OL_segment_onset(j) = segment_offset;            // Change rows overlap
          segment_offset = segment_onset_vec(j);                // change evaluated row   [ !!!!!! in the R code here was a 'max()' function because there was a vector of values whereas I have a single value ]
        }
      }

      LENA_OL_segment_onset(i) = segment_onset;                 // modification of lena row
      LENA_OL_segment_offset(i) = segment_offset;
    }
  }

  return Rcpp::List::create(Rcpp::Named("LENA_OL_segment_onset") = LENA_OL_segment_onset,
                            Rcpp::Named("LENA_OL_segment_offset") = LENA_OL_segment_offset);
}

