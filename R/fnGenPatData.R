# Helper function for use in lSimBase or lPatOutcome
# Programmed by: Dariga Ramazanova

fnGenPatData <- 
  function(
    n,
    vars, # 
    cnames = c(),
    long_data = "no", # yes, cdisk
    dataset = NULL, 
    id = NULL,
    ...
  ) {
    
    # extract number of variables
    n_var <- length(vars)
    
    if (n_var == 0) {
      stop('ERROR: vars is empty')
    }
    
    # errors#####
    # check whether the input is of right class/mode:
    #               - elements in vars are chr ........ done
    #               - list of lists ................... done
    #               - vars has at least one element ... done
    #               - names of vars do not repeat ..... done
    #               - #"(" = #")" ..................... done
    #               - extra arguments ................. 
    
    # do we need to check it? if yes - how?
    #       - in case of multidimensional variable: number of dimensions must be written after the function/formula
    #       - more than 3 elements in each list of vars?
    #       - n must be mentioned for every variable simulated using the R function that requires n
    #
    
    # n
    if (!is.numeric(n) | !is.vector(n)) {
      stop('ERROR: n must be a numeric value')
    }
    
    if (length(n) > 1) {
      n <- n[1]
      warning('WARNING: the first element of n was taken')
    }
    
    if (n %% 1 != 0) {
      floor(n)
      warning('WARNING: n was rounded down')
    }
    
    # vars
    #stopifnot(sapply(vars, is.list))
    if (!is.list(vars) | any(!sapply(vars, is.list))) {
      stop('ERROR: vars must be a list of lists')
    }
    
    if (any(sapply(vars, length) < 2)) {
      stop('ERROR: each list of vars must contain at least 2 elements: name of variable and function/formula')
    }
    
    # check whether the first 2 elements of list is character
    bool <- vector(mode = 'logical', length = n_var)
    for (i in 1:n_var) {
      bool[i] <- all(sapply(vars[[i]], is.character)[1:2])
    }
    if (any(!bool)) {
      stop('ERROR vars: name and function/formula must be character') 
    }
    
    # cnames
    if (!is.character(cnames) & !is.null(cnames)) {
      stop('ERROR: cnames must be a character value/vector')
    }
    
    #long_data <- tolower(long_data)
    if (grepl("yes", long_data)) {
      if (sum(grepl("_", cnames)) < 2) {
        stop('ERROR: for long datasets, at least 2 cnames must be contain "_"')
      }
    }
    
    # long data
    if (!is.character(long_data) | !grepl('^no$|^yes$|^cdisk$', long_data)) {
      stop('ERROR: long_data must be one of three options - "no", "yes", or "cdisk"')
    }
    
    # dataset
    bool_dataset <- !is.null(dataset)
    
    if (bool_dataset & !is.data.frame(dataset)) {
      stop('ERROR: dataset must be a data frame')
    }
    
    
    
    # logical&errors####
    
    # whether the data should be simulated based on another data set
    n_dat <- ifelse(bool_dataset, length(dataset), 0)
    all_sim_var <- vector("list", length = n_var + n_dat)
    
    if (bool_dataset) {
      names(all_sim_var) <- c(names(dataset), sapply(vars, "[[", 1))
      temp_functions <- c(rep(NA, n_dat), sapply(vars, "[[", 2))
      all_sim_var[1:n_dat] <- dataset
    } else {
      names(all_sim_var) <- sapply(vars, "[[", 1)
      temp_functions <- c(rep(NA, n_dat), sapply(vars, "[[", 2))
    }
    
    # ERRORS
    if (any(table(as.factor(names(all_sim_var))) > 1)) {
      stop('ERROR: variable names must be different')
    }
    
    
    
    # any_extra_arg <- ...length()
    #match.call(expand.dots = FALSE)
    
    #if (length(list(...))) FALSE else TRUE
    if (length(list(...))) { #  && !is.null(...)
      extra_arg <- list(...) #list(z = 37, lll = c(1, 20)) # !!!!!!!!
      bool_extra_arg <- sapply(vars, '[[', 1) %in% sapply(extra_arg, '[[', 2)
      if (any(bool_extra_arg)) {
        extra_arg_values <- sapply(extra_arg, '[[', 1)
        extra_arg_vars <- sapply(extra_arg, '[[', 2)
        #extra_arg_pos <- sapply(extra_arg, '[[', 3) 
        # whether variables have more than 1 extra_arg
        bool_only1_extra_arg <- table(sapply(extra_arg, '[[', 2)) == 1
      }
    } else {
      bool_extra_arg <- bool_only1_extra_arg <- rep(FALSE, n_var + n_dat) 
    }
    
    #is_extra_arg_vector <- sapply(extra_arg, length) > 1 #!!!!!!!!
    
    
    
    
    # to check whether number of parentheses is equal
    bool <- length(grep('[(]', temp_functions)) != length(grep('[)]', temp_functions))
    if (bool) {
      stop('ERROR: the number of parentheses is unequal (check the formulas/functions)')
    }
    
    ndim <- rep(1, n_var + n_dat)
    # to check whether there are multidim variables to simulate
    bool_multidim <- c(rep(FALSE, n_dat), sapply(vars, length) > 2)
    if (any(bool_multidim)) {
      vek <- sapply(vars[bool_multidim[(n_dat + 1):length(bool_multidim)]], "[[", 3)
      
      if (any(!is.numeric(vek))) {
        stop('ERROR: The 3rd elements of lists in vars must be numeric')
      }
      
      length_3rd_element <- length(unlist(sapply(vars[bool_multidim[(n_dat + 1):length(bool_multidim)]], "[[", 3)))
      if (length_3rd_element != sum(bool_multidim)) {
        stop('ERROR: number of dimensions must be a single value')
      }
      
      if (any((vek %% 1) != 0)) {
        floor(vek)
        warning('WARNING: number of dimensions was rounded down')
      }
      
      ndim[bool_multidim] <- as.numeric(sapply(vars[bool_multidim[(n_dat + 1):length(bool_multidim)]], "[[", 3))
      
    }
    
    
    # logical: to check whether the covariates are based on the covariates
    # "[^a-z, A-Z]varname", " varname ", "(varname)" 
    var_from_var <- grepl(paste0("\\<", names(all_sim_var), "\\>", collapse = '|'), temp_functions) # &
    # grepl("[+|-|*|/|^]", temp_functions)
    # grepl(paste0(".", names(all_sim_var), collapse = "|"), temp_functions)
    # paste0("[^a-z, A-Z]", names(all_sim_var), collapse = "|")
    
    # check whether vars are in right order if one of them######
    if (any(var_from_var)) {
      if (var_from_var[1]) {
        stop("ERROR: the 1st variable cannot be based on the previous one")
        # problem: var name - p, function - sample
      } else {
        ind <- which(var_from_var)
        for (i in ind) {
          bool <- grepl(paste0(names(all_sim_var)[(i + 1):length(all_sim_var)], collapse = "|"), temp_functions[i])
          if (any(bool)) {
            stop("ERROR: the order of variables is incorrect (at least 1 variable is based on others)")
          }
        }
      }
      # grepl(paste0(names(all_sim_var)[(ind + 1):length(all_sim_var)], collapse = "|"), temp_functions[ind])
    }
    
    bool_per_pat <- !grepl("[(]n[)],[(]n,|n,", temp_functions) & bool_multidim
    # not very optimal solution --> how to read whether they are for each patient?
    
    
    # main for-loop####
    # for-loop for simulating variables based on formula OR r function
    # j <- 1 # formula must be based on previous variables
    for (iVars in 1:n_var) {
      # iVars <- 1
      
      var_formula <- temp_functions[iVars + n_dat]
      
      # to connect variables based on the previous ones with list elements in formula
      if (var_from_var[iVars + n_dat]) {
        for (j in 1:(iVars + n_dat)) {
          var_formula <- gsub(paste0("\\<", names(all_sim_var)[j], "\\>"), 
                              paste0("all_sim_var$", names(all_sim_var)[j]), 
                              var_formula)
          var_formula <- gsub("all_sim_var[$]all_sim_var[$]", "all_sim_var$", var_formula)
        }
      }
      
      # to simulate the variables without any extra arguments
      if (!bool_extra_arg[iVars + n_dat]) {
        
        # to simulate the values for each patient separately
        if (bool_per_pat[iVars + n_dat]) {
          for (j in 1:(iVars + n_dat)) {
            # to differentiate between the whole sample and each pat
            # naive assumption: from all pat - formula with () around cov name
            bool <- grepl(paste0("[(]all_sim_var\\$", names(all_sim_var)[j], "[)]"), var_formula)
            # grepl(paste0("[(all_sim_var$", names(all_sim_var)[j], ")]"), var_formula)
            # v1: grepl(paste0("(all_sim_var\\$", names(all_sim_var)[j], ")"), var_formula)
            if (bool) {
              # option 1
              # var_formula <- gsub(paste0(names(all_sim_var)[j], ")"),
              #                     paste0(names(all_sim_var)[j], ')[ipat]'), var_formula)
              # var_formula <- gsub(names(all_sim_var)[j], paste0(names(all_sim_var)[j], '[ipat]'), var_formula)
              # var_formula <- gsub("\\[ipat\\])\\[ipat\\]", ')', var_formula)
              # option 2
              var_formula <- gsub(paste0(names(all_sim_var)[j], "[)]"), 
                                  paste0(names(all_sim_var)[j], ")[ipat]"), 
                                  var_formula)
              #var_formula <- gsub("\\[ipat\\][)]", ")", var_formula)
              var_formula <- gsub("\\[ipat\\]\\[ipat\\]", "[ipat]", var_formula)
            } else {
              var_formula <- gsub(names(all_sim_var)[j], paste0(names(all_sim_var)[j], "[ipat]"), var_formula)
            }
          }
          l <- matrix(numeric(n * ndim[iVars + n_dat]), ncol = ndim[iVars + n_dat])
          for (ipat in 1:n) { # for each patient
            l[ipat, ] <- eval(parse(text = var_formula))
            # mvtnorm::rmvnorm(1, mean = c(all_sim_var$age[ipat] * 0.9, all_sim_var$status[ipat] * 0.5), sigma = matrix(c(0.3 * all_sim_var$age[ipat], -5, -5, 64 + all_sim_var$status[ipat]), ncol = 2))
          }
          all_sim_var[[iVars + n_dat]] <- l
        } else {
          # to simulate the values for the whole sample
          
          all_sim_var[[iVars + n_dat]] <- eval(parse(text = var_formula))
        }
        
      } else { # if any extra_arg
        # to simulate the variables with extra arguments
        
        # to simulate the values for each patient separately
        if (bool_per_pat[iVars + n_dat]) {
          print('not implemented for each patient')
          next()
        } else { # for now: only for functions
          # to simulate the values for the whole sample
          
          if (bool_only1_extra_arg[names(all_sim_var)[iVars + n_dat]]) {
            ind_extra_arg <- grep(names(all_sim_var)[iVars + n_dat], extra_arg_vars)
            assign(names(extra_arg)[ind_extra_arg], extra_arg_values[[ind_extra_arg]])
            #all_sim_var[[i]] <- eval(parse(text = var_formula))
          } else {
            for (iArgs in grep(names(all_sim_var)[iVars + n_dat], extra_arg_vars)) {
              assign(names(extra_arg)[iArgs], extra_arg_values[[iArgs]])
            }
            #all_sim_var[[i]] <- eval(parse(text = var_formula))
          }
          all_sim_var[[iVars + n_dat]] <- eval(parse(text = var_formula))
          
        }
        
      }
      #}
      
    }
    
    # check if number of dimensions is correct
    # option 1: sapply(all_sim_var, length) / n
    # option 2:
    Ndim <- lapply(all_sim_var, dim)
    bool_unidim <- sapply(Ndim, is.null)
    if (any(ndim[bool_unidim] != 1)) {
      warning(paste('WARNING: number of dimensions is incorrect. Variable(-s)', 
                    paste(names(all_sim_var)[ndim[bool_unidim] != 1], collapse = ','), 
                    'is/are 1-dimensional.'))
    }
    
    if (any(!bool_unidim)) {
      bool <- sapply(Ndim[!bool_unidim], '[', 2) != ndim[!bool_unidim]
      if (any(bool))
        warning(paste('WARNING: number of dimensions is incorrect. Check variable(-s)', 
                      paste(names(bool)[bool], collapse = ',')))
    }
    
    # for the output as a data frame
    # if there are some variables drawn from multidim. dist.
    if (any(sapply(all_sim_var, is.matrix))) { 
      temp_list <- vector(mode = "list", length = 0)
      
      for (i in 1:length(all_sim_var)) {
        # if (is.null(all_sim_var[[i]])) { # what for?
        #   temp_list <- c(temp_list, all_sim_var[[i]])
        # } else {
        #   temp_list <- c(temp_list, as.list(as.data.frame(all_sim_var[[i]]))) 
        #   # w/o as.list(as.data.frame()) - as list of lists
        # }
        to_bind <- as.data.frame(all_sim_var[[i]])
        if (length(to_bind) == 1) {
          names(to_bind) <- names(all_sim_var)[i]
        } else {
          names(to_bind) <- paste0(names(all_sim_var)[i], 1:length(to_bind))
        }
        temp_list <- c(temp_list, to_bind)
        
      }
      
      temp_list <- as.data.frame(temp_list)
    } else {
      temp_list <- as.data.frame(all_sim_var)
      names(temp_list) <- names(all_sim_var)
    }
    
    
    # cnames####
    if (is.null(cnames)) {
      # if (length(names(all_sim_var)) == length(temp_list)) {
      #   names(temp_list) <- names(all_sim_var)
      # } else {
      #   bool_matrix <- sapply(all_sim_var, is.matrix)
      #   names(temp_list)[!bool_matrix] <- names(all_sim_var)[!bool_matrix]
      #   names(temp_list)[bool_matrix]
      # }
      cnames <- names(temp_list)
      #cnames <- paste0('Var', 1:length(temp_list))
      warning('Column names were created automatically')
    }
    
    # !!!!!!!!!!!!!!!!! ID is not taken into account
    # n_colnames_id <- ifelse(!any(grepl('ID', names(temp_list))),
    #                         n_var + n_dat,
    #                         n_var + n_dat - 1)
    # if (length(cnames) != n_colnames_id) {
    #   if (length(cnames) > n_colnames_id) {
    #     cnames <- cnames[1:(n_colnames_id)]
    #   } else {
    #     cnames <- c(cnames, paste0('Var', (length(cnames) + 1):(n_var + n_dat)))
    #   }
    #   
    #   warning('Length of cnames is not equal to number of variables')
    # }
    
    # ind <- ifelse(!any(grepl('ID', names(temp_list))), 1, 2)
    # 
    # if (length(cnames) != length(temp_list)) {
    #   if (length(cnames) > length(temp_list)) {
    #     cnames <- cnames[ind:length(temp_list)]
    #   } else {
    #     #cnames <- c(cnames, paste0('Var', (length(cnames) + 1):length(temp_list)))
    #     #!!!!!!!!!!!!!!!!!!! MULTIDIM
    #     cnames <- c(cnames, names(all_sim_var)[(length(cnames) + 1):length(temp_list)])
    #   }
    #   
    #   warning('Length of cnames is not equal to number of variables')
    # }
    # 
    # OUTPUT <- ifelse(!any(grepl('ID', names(temp_list))), 
    #                  data.frame(ID = 1:n, as.data.frame(temp_list)), 
    #                  as.data.frame(temp_list))
    # colnames(OUTPUT) <- ifelse(!any(grepl('ID', names(temp_list))), c("ID", cnames), cnames)
    
    if (is.null(id)) {
      if (!any(grepl('ID', names(temp_list)))) {
        if (length(cnames) != length(temp_list)) {
          if (length(cnames) > length(temp_list)) {
            cnames <- cnames[1:length(temp_list)]
          } else {
            #cnames <- c(cnames, paste0('Var', (length(cnames) + 1):length(temp_list)))
            #!!!!!!!!!!!!!!!!!!! MULTIDIM
            cnames <- c(cnames, names(temp_list)[(length(cnames) + 1):length(temp_list)])
          }
          
          warning('Length of cnames is not equal to number of variables')
        }
        
        OUTPUT <- data.frame(ID = 1:n, as.data.frame(temp_list))
        colnames(OUTPUT) <- c("ID", cnames)
      } else {
        if (length(cnames) != length(temp_list) - 1) {
          if (length(cnames) > length(temp_list) - 1) {
            cnames <- cnames[2:length(temp_list)]
          } else {
            cnames <- c(cnames, names(temp_list)[(length(cnames) + 1):length(temp_list)])
          }
          
          warning('Length of cnames is not equal to number of variables')
        } else {
          cnames <- c('ID', cnames)
        }
        OUTPUT <- as.data.frame(temp_list)
        colnames(OUTPUT) <- cnames
      }
    } else {
      OUTPUT <- as.data.frame(temp_list)
      colnames(OUTPUT) <- cnames
    }
    
    
    
    #colnames(OUTPUT) <- c("ID", cnames)
    
    # long data####
    require(tidyr)
    if (grepl("yes", long_data)) {
      OUTPUT <- OUTPUT %>%
        tidyr::pivot_longer(grep("_[0-9]", names(OUTPUT)),
                            names_to = c(".value", "visit"),
                            names_sep = "_",
                            values_drop_na = TRUE
        )
    }
    if (grepl("cdisk", long_data)) {
      OUTPUT <- OUTPUT %>%
        tidyr::pivot_longer(!ID, names_to = "name", values_to = "value")
    }
    
    
    return(OUTPUT)
    #list(OUTPUT, extra_arg = extra_arg, is_extra_arg_vector = is_extra_arg_vector, bool_extra_arg = bool_extra_arg)
  }
