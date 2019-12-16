
# Function to convert PLS-5 raw scores to/from growth scale values (GSVs)
gsv_convert <- function(input, type, out, new=c()){
  
  # Receptive
  if (type=="receptive"){
    convert_tbl <- data.frame(raw = c(0:65), gsv = c(96, 127, 153,
                                                     175, 194, 207,
                                                     218, 227, 236,
                                                     245, 254, 262,
                                                     271, 280, 288,
                                                     296, 304, 313,
                                                     321, 332, 342,
                                                     351, 358, 364,
                                                     369, 374, 379,
                                                     384, 388, 393,
                                                     397, 402, 408,
                                                     414, 420, 425,
                                                     431, 436, 440,
                                                     445, 449, 453,
                                                     457, 460, 464,
                                                     467, 470, 473,
                                                     476, 478, 481,
                                                     484, 486, 489,
                                                     492, 495, 498,
                                                     501, 504, 508,
                                                     511, 515, 520,
                                                     526, 535, 543))
    new <- c()
    if (out=="gsv"){
      for (i in 1:length(input)){new <- c(new, convert_tbl$gsv[convert_tbl$raw==input[i]])}
    } else if(out=="raw"){
      for (i in 1:length(input)){new <- c(new, convert_tbl$raw[convert_tbl$gsv==input[i]])}
    }
  }
  
  # Expressive
  if (type=="expressive"){
    convert_tbl <- data.frame(raw = c(0:67), gsv = c(157, 171, 183,
                                                     193, 204, 216,
                                                     230, 242, 251,
                                                     258, 264, 270,
                                                     275, 280, 285,
                                                     290, 296, 302,
                                                     307, 313, 318,
                                                     323, 328, 334,
                                                     341, 349, 359,
                                                     367, 374, 380,
                                                     386, 393, 400,
                                                     409, 417, 426,
                                                     433, 440, 446,
                                                     452, 457, 462,
                                                     466, 469, 473,
                                                     476, 479, 483,
                                                     486, 489, 491,
                                                     494, 497, 500,
                                                     503, 505, 508,
                                                     511, 513, 516,
                                                     519, 522, 525,
                                                     529, 534, 539,
                                                     548, 556))
    new <- c()
    if (out=="gsv"){
      for (i in 1:length(input)){new <- c(new, convert_tbl$gsv[convert_tbl$raw==input[i]])}
    } else if(out=="raw"){
      for (i in 1:length(input)){new <- c(new, convert_tbl$raw[convert_tbl$gsv==input[i]])}
    }
  }
  
  return(new)
}


# Simulated raw data
exp_raw <- c(34, 37, 39, 42, 35, 40, 28, 35, 40, 41, 36, 42, 41, 35, 29, 29, 39, 36, 40, 44)
recp_raw <- c(40, 35, 41, 43, 39, 42, 27, 38, 43, 42, 35, 39, 38, 40, 38, 31, 38, 43, 43, 48)

# How to use the function to get gsv from raw scores
exp_gsv <- gsv_convert(input=exp_raw, out="gsv", type="expressive")
exp_gsv
recp_gsv <- gsv_convert(input=recp_raw, out="gsv", type="receptive")
recp_gsv

# How to use the function to get raw scores from gsv
gsv_convert(input=exp_gsv, out="raw", type="expressive")
gsv_convert(input=recp_gsv, out="raw", type="receptive")
