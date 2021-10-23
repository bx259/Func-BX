# Old version(s)

# #crate a function for adding labels and footnote for variables of interest
# tbl1_prep <- function(
#   cat_var,cat_lab,
#   cat_fn,df) {
#   #set labels (w/ footnot)
#   for (i in cat_var) {
#     #label j corresponding to var i
#     j <- cat_lab[match(i,cat_var)]
#     #set labels
#     df[,i] <-
#       df %>%
#       pull(i) %>%
#       setLabel(ifelse(j %in% cat_fn,
#                       paste0(j,
#                              footnote_marker_symbol(match(j,cat_fn))),
#                       j))
#   }
#   temp <- list(df,cat_lab)
#   return(temp)
#   #to use in style
# }

#
# # format and style
#
# tb1_style <- function(
#   cap = 'Patient Demographics',
#   fn_txt, cat_lab,df,
#   font = 'Arial',size = 11
# ){
#   df %>%
#     kbl(caption = cap,
#         escape = FALSE) %>%
#     #escape = F is to keep special characteristics from escaping
#     kable_classic(full_width = F,
#                   html_font = font) %>%
#     kable_styling(bootstrap_options = c("striped", "hover",
#                                         "condensed", "responsive"),
#                   font_size = size,
#                   full_width = FALSE) %>%
#     row_spec(c(0, charmatch(cat_lab,df[,1])),
#              bold = T) %>%
#     footnote(
#       symbol = fn_txt,
#       footnote_as_chunk = FALSE,
#       fixed_small_size = TRUE)
#
# }

###################################################
########Basic Formatting based on kableExtra#######
###################################################

fmt_BX <- function(
  .df,fn_txt = NULL,
  font = 'arial',size = 11,
  classic = "Y",...){
  dots = list(...)
  #if classic
  if (classic == 'Y') {
    temp <-
      rlang::expr(kableExtra::kable_classic(
        kable_input = !!.df,
        lightable_options  = c( "hover",'striped',
                                "condensed", "responsive"),
        font_size = !!size,html_font = !!font,full_width = FALSE,
        fixed_thead = TRUE,!!!dots)) %>%
      eval() %>%
      # common HTML css font
      kableExtra::kable_styling()
    }else{
      temp <-
        rlang::expr(kableExtra::kable_styling(
          kable_input = !!.df,
          bootstrap_options = c( "hover",'striped',
                                 "condensed", "responsive"),
          font_size = !!size,html_font = !!font,full_width = FALSE,
          fixed_thead = TRUE,!!!dots)) %>%
        eval()
        # common HTML css font
    }

  #add footnote, if requested
  if (!is.null(fn_txt)) {
    temp %>% footnote(
        symbol = fn_txt,
        footnote_as_chunk = FALSE,
        fixed_small_size = TRUE)
  }else{
    return(temp)
  }
  }




# New version

############################
##########Func Doc#E########
############################


# 1.
# Have to make sure length(row_var) == length(row_lab)

# 2.
# col_var does not require labs cuz it's usually
# defined as levels of the strata/col variables

# Action:
# will need to add feasure of summarizing nonnorm variable
tb1_BX <- function(
  row_var,row_lab,
  row_fn,.df,
  col_var = NULL,nonnorm_var = NULL,
  p_value = FALSE,fn_txt = NULL,
  cap = 'Patient Demographics',
  font = 'arial',size = 11,classic = "Y") {
  # as groups lose when converting to df, we don't define groups here
  # groups=list("", "", ""))
  # Add lables/units to row variabels
  row_name <- list()
  for (i in row_var) {
    #label j corresponding to var i
    j <- row_lab[match(i,row_var)]
    #set labels
    row_name[['variables']][[i]] <-
      ifelse(j %in% row_fn,
                      paste0(j,
                             footnote_marker_symbol(match(j,row_fn))),
                      j)
  }

  # Define/add column variables

  df_s <- .df %>%
    select(row_var,col_var)
  #get a 'S' version of df only including var of interest

  if (is.null(col_var)) {
    #no strata variable specified
    col_name <- list('Total' = df_s)
  } else{
    #strata variable specified with total column added
    col_name <- c(list('Total' = df_s),
                  split(df_s,pull(df_s,col_var)))
  }
  
  #define render functions (for nonnormal)
  render.cont <-
    function(x) {
      with(lapply(stats.default(x),
                  function(i){round(i,digits = 1)}), 
           c("","Median [Min, Max]"=sprintf("%s [%s, %s]", MEDIAN, MIN,MAX)))
    }
  
  #creat table 1
  tb <- table1(col_name,row_name,
               render.continuous=render.cont) %>%
    #skip groupspan as we didn't specify groupings
    as.data.frame()
  
  #pull p-values through CreateTableOne
  
  #set nulls 
  pvalue1 <- c(rep("",nrow(tb)))
  
  #all the p-values
  pvalue2 <- 
    print(CreateTableOne(vars = c(row_var,col_var), 
                         strata = col_var, 
                         data = df_s),
          nonnormal = nonnorm_var, 
          exact = nonnorm_var,
          formatOptions = list(big.mark = ",")) %>%
    as.data.frame() %>%
    filter(p != '') %>%
    pull(p) 
  
  #remove the p-value for comparing strata itself
  pvalue2 <- pvalue2[-length(pvalue2)]
  
  #p-values and blanks
  pvalue1[charmatch(row_lab,tb[,1])] <-
    pvalue2
  
  #add p-value or not
  if (p_value) {
    tb <- tb %>%
      mutate("P-value" = pvalue1)
  }
  
  # format and style
  tb %>%
    kbl(caption = cap,
        escape = FALSE) %>%
    fmt_BX(fn_txt,font,size,classic) %>%
    # table 1 exclusive
    row_spec(charmatch(row_lab,tb[,1]),
             bold = T,italic = FALSE) %>%
    row_spec(0, color = 'white',background = "#666",bold = T)



  # %>%
  #   scroll_box(height = tbl_ht)

}

# MSK colors
# "#DF4602" "#007CBA"

# Below shows that we can create a stand-alone footnote chunk
# c() %>% tibble()  %>%
#   kbl() %>%
#   footnote(symbol = "Footnote Symbol 1; ")

try <- function(i)
{
  return(i)
}



