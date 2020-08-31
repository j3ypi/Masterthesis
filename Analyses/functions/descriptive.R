descriptive <- function(data, .round = 2) {
  
  desc <- function(data, .round = .round) {
    data %>% 
      select_if(is.numeric) %>% 
      pivot_longer(
        cols = everything(),
        names_to = "Variable",
        values_to = "Value"
      ) %>% 
      group_by(Variable) %>% 
      summarise(N = n(), 
                Min = round(min(Value, na.rm = TRUE), .round),
                Q1 = round(quantile(Value, 0.25, na.rm = TRUE), .round),
                Mean = round(mean(Value, na.rm = TRUE), .round),
                Median = round(median(Value, na.rm = TRUE), .round),
                Q3 = round(quantile(Value, 0.75, na.rm = TRUE), .round),
                Max = round(max(Value, na.rm = TRUE), .round),
                SD = round(sd(Value, na.rm = TRUE), .round),
                SE = round(SD / sqrt(N), .round))
  }
  if (any(class(data) == "data.frame") & !is_grouped_df(data)) {
    data %>% 
      desc(.round) %>% 
      as.data.frame()
  } else if (is_grouped_df(data)) {
    data %>% 
      group_modify(~ desc(.x, .round)) %>% 
      as.data.frame()
  } else {
    stop("The data must be a data.frame, tibble or grouped.df", 
         call. = FALSE)
  }
}