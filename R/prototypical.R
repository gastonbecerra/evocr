
prototypical_analysis <- function(x,
                                  terms,
                                  order,
                                  min_threshold = 2,
                                  order_cut_off = "mean",
                                  freq_cut_off = "mean",
                                  valoracion) {

  # x, terms y order son la base
  # terms y order defaults to col1 and col2
  # otros, como valoracion, deberian venir con una funcion de agregación, mean por default
  # cortes hay que darlos por funcion o por numero: mean, median, threshold, zipf, half1st
  # min_threshold?
  # freq-dist se tiene que poder obviar

  stopifnot(is.data.frame(x))
  stopifnot(is.character(terms))
  stopifnot(is.numeric(min_threshold))

  terms_column <- enquo(arg = terms)
  order_column <- enquo(arg = order)
  valor_column <- enquo(arg = valoracion)

  # frequency x rank table

  freq_x_rank <- x %>%
    group_by(!!terms_column) %>%
    summarise(
      freq = n(), # frecuencia
      rank = mean(!!order_column), # media de order de evocacion
      assessm = mean(!!valor_column), # media de valoracion
      .groups = "drop_last"
    )

  # le calculamos una frecuencia minima
  # otra opcion es hacer los calculos sobre el corpus no cortado

  freq_x_rank <- freq_x_rank %>%
    group_by(!!terms_column) %>%
    filter(freq >= min_threshold)

  # modo prototypical de iramuteq
  # x <- freq_x_rank %>%
  #   column_to_rownames(var = "terms") %>%
  #   select(freq,rank)
  # mfreq <- sum(x[,1]) / nrow(x)
  # mrank <- sum(x[,1] * x[,2]) / sum(x[,1])
  # print(mfreq)
  # print(mrank)

  # 2do: permitir meter las operaciones (o los valores) como args

  freq_cut <- mean(freq_x_rank$freq) # cut-off media de frecuencias de evocacion
  rank_cut <- mean(freq_x_rank$rank) # cut-off between high and low rank = a mean split was used (De Rosa, 2003)

  # rank cut esta dando != que iramuteq

  message("frequency cut = ", freq_cut)
  message("rank cut = ", rank_cut)

  freq_x_rank <- freq_x_rank %>%
    mutate(q = case_when(
      freq >= freq_cut & rank < rank_cut ~ 1,
      freq >= freq_cut & rank >= rank_cut ~ 2,
      freq < freq_cut & rank < rank_cut ~ 3,
      freq < freq_cut & rank >= rank_cut ~ 4 # teoricamente, no tiene sentido distinguir el 3 del 4
    )) %>%
    arrange(q, desc(freq, rank))

  return(
    list(
      data = freq_x_rank,
      parameters = list(
        "freq_cut" = freq_cut,
        "rank_cut" = rank_cut,
        "min_cut" = min_threshold,
        "dix_length" = nrow(freq_x_rank),
        "q1_length" = sum(freq_x_rank$q == 1),
        "q2_length" = sum(freq_x_rank$q == 2),
        "q3_length" = sum(freq_x_rank$q == 3),
        "q4_length" = sum(freq_x_rank$q == 4)
      )
    )
  )
}
