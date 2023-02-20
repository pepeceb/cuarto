catchdata_transformation <- function(data){
  names(data)<-c("ship_ID","stock","landings")

  data <- data %>%
    mutate(ship_ID =as.character(ship_ID))

  stocks_sum <- data %>%
    dplyr::group_by(ship_ID, stock) %>%
    dplyr::summarise(weight_landed = sum(landings)) %>%
    dplyr::group_by(ship_ID)%>%
    dplyr::mutate(share_stock= weight_landed/sum(weight_landed)) %>%
    dplyr::ungroup()

  stocks_sum <- stocks_sum[,-ncol(stocks_sum)]
  stocks_wide <- spread(stocks_sum, key = "stock", value = weight_landed, fill = 0)
  stocks_wide <- as.data.frame(stocks_wide)
  rownames(stocks_wide) <- stocks_wide$ship_ID
  stocks_wide <- stocks_wide[,-c(1)]

  catchdata <- stocks_wide/rowSums(stocks_wide)
  if(ncol(catchdata) == 0){
    warning("Your transformed catchdata does not contain any columns. Are you sure, your original data included more than one stock and more than one vessel?")
  }
  return(catchdata)
}