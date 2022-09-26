# Helper function that finds unique tokens in a sentence
tkn_count <-  function(token){
  if(exists("counter")){
    if(!str_detect(token, "##")){
      tkn_n = counter
      counter <<- tkn_n + 1
    } else{
      tkn_n = counter - 1
    }
    
  }else{
    counter <<- 1 
    if(!str_detect(token, "##")){
      tkn_n = counter
      counter <<- tkn_n + 1
    } else{
      tkn_n = counter - 1
    }
    
  }
  # Remove after every sentence
  if(token == "[SEP]"){
    rm(counter, pos = globalenv())
  }
  
  
  return(tkn_n)
}

# Function that splits atttributions into a tibble
# finds mean attributions on a word basis
add_scores <- function(tokens){
  tokens %>% 
    str_split(" \\(") %>% 
    unlist() %>% 
    tibble(tokens = ., ) %>%
    mutate(rn = row_number()) %>% 
    group_by(rn) %>% 
    mutate(
      tokens = str_split(tokens, ", ", n = 2) %>% 
        unlist() %>% 
        str_c(collapse = "!!")) %>% 
    separate(col = tokens, into = c("tokens", "attr_scores"), sep = "!!") %>%
    ungroup() %>% 
    mutate(
      # Parse numbers for attribution scores
      attr_scores = parse_number(attr_scores),
      
      # Make numeric to be double for calculations
      across(where(is.numeric), as.double),
      
      # Remove quotes at the start and end of string
      tokens = case_when(
        tokens == "\"'\"" ~ str_extract(tokens, "[']"),
        TRUE ~ str_remove_all(tokens, "['']"))) %>% 
    #tokens %>% str_remove("^[']")) %>% 
    group_by(rn) %>% 
    mutate(unq_tokens = tkn_count(tokens)) %>% 
    ungroup() %>% 
    group_by(unq_tokens) %>% 
    summarize(tokens = str_c(tokens, collapse = "") %>% str_remove_all("##"),
              attr_scores = mean(attr_scores)) %>% 
    ungroup()
}

# Function that calculates distinct keywords and distinctiveness measure
distinct_measure <- function(tbl_list){
  if(is.list(tbl_list)){ # Not working, df is a list?
    tbl_list %>% 
      bind_rows() %>%
      add_count(tag, name = "tag_count") %>% 
      # Remove key words that occur in more than 1 tag ie duplicated
      filter(!(tokens %in% keep(.$tokens, duplicated(.$tokens)))) %>%
      add_count(tag, name = "dst_tag_count") %>% 
      mutate(distinct_ratio = dst_tag_count/tag_count) %>% 
      distinct(tag, distinct_ratio) %>% 
      mutate(distinct_measure = mean(distinct_ratio))
  } else{
    stop("input is not a list")
  }
  
}


# Read and bind similar files
read_bind_files <- function(path, pattern){
  # Read files
  files = list.files(
    path,
    pattern = pattern, 
    recursive = TRUE,
    full.names = TRUE
  ) 
  
  # Add a model and resample identifier
  files = map(seq_len(length(files)), ~ read_csv(files[.x], show_col_types = FALSE) %>% 
                mutate(res_id = factor(.x))) %>% 
    bind_rows()
  
  return(files)
}