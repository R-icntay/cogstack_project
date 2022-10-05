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

# Calculating class distinctiveness (intrinsic)
distinct_measure <- function(kw_list){
  
  # Helper function to calculate number of distinct keywords of an entity
  calc_distinct_keywords = function(tag, all_kws){
    discard(all_kws %>% filter(tag == {{tag}}) %>% pull(tokens),
            (all_kws %>% filter(tag == {{tag}}) %>% pull(tokens)) %in% (all_kws %>% filter(tag != {{tag}}) %>% pull(tokens))) %>% length(.) %>%  tibble(tag = tag, distinct_kws = .)
  }
  
  # Tibble of all keywords
  all_kws = kw_list %>% 
    bind_rows() %>% 
    select(tokens, tag) %>% 
    add_count(tag, name = "kws_in_class")
  
  # Vector of all tags
  all_tags = unique(all_kws$tag)
  
  # Calculate the distinctiveness of entity keywords
  purrr::map_dfr(all_tags, ~ calc_distinct_keywords(.x, all_kws = all_kws)) %>% 
    left_join(all_kws %>% distinct(tag, kws_in_class), by = "tag") %>% 
    relocate(kws_in_class, .after = tag) %>% 
    mutate(class_dm = distinct_kws / kws_in_class) %>% 
    mutate(overall_dm = mean(class_dm))
  
  
}




# Function that takes a tibble of attributions and corresponding key words
# and calculates coverage
coverage_measure <- function(attributions, key_words){
  attributions %>%
    # Remove tokens e.g SEP, CLS
    filter(attr_scores != 0) %>%
    # Account for repeating sentences
    group_by(res_id, sentence_id, row_id) %>% 
    # Find sentence tokens which are in the keywords
    filter(tokens %in% key_words) %>% 
    # Find how many they are
    mutate(kw_in_sntnc = n()) %>% 
    ungroup() %>% 
    # Calculate coverage
    mutate(total_kw = length(key_words),
           coverage = kw_in_sntnc/total_kw) %>% 
    distinct(sentence_id, coverage) %>% 
    summarise(tag = distinct(attributions, tag) %>% pull(tag), coverage = mean(coverage)) %>% 
    ungroup()
  
}
