
getFunArgNames <- function(pkg){
  funs_txt <- lsf.str(paste0("package:", pkg)) %>% capture.output() %>%
    paste0(collapse = " ")
  
  fun_nms <- funs_txt  %>% 
    stringr::str_extract_all("\\S*(?= \\: )") %>% unlist() %>% 
    stringr::str_subset("..*")
  
  fun_type <- funs_txt  %>% 
    stringr::str_extract_all("(?<= \\: )\\S*") %>% unlist()
  
  s4_args <- function(fun_nm){
    findMethods(fun_nm) %>% capture.output() %>% paste0(collapse = " ") %>% 
      stringr::str_extract("(?<=\\.local .. function )[(][^}{]*[)]") %>% 
      stringr::str_split(",") %>% unlist() %>% 
      stringr::str_remove("\\(|\\)") %>% stringr::str_remove("=.*$") %>% stringr::str_trim()
  }
  
  data.frame(name = fun_nms, type = fun_type) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(args = ifelse(type == "function", list(formalArgs(name)), 
                                list(s4_args(name)))) %>% 
    tidyr::unnest(args)
}




