# author : sai
# PPE automation


automate <- function(final_offers,top_banks, hide_banks, bigbank_cutoff, WB_name) {
  
  ## Garbage collection ##
  gc()
  
  ### creating bank flags ###
  for (i in 1:length(top_banks)) {
    final_offers[[top_banks[[i]]]] <- ifelse(final_offers$bank_name.s == top_banks[[i]],1,0)
  }

  ### aggregating ###
  library(data.table)
  final_offers <- data.table(final_offers)
  setkey(final_offers, eligibility_id)
  aggr <- as.data.frame(final_offers[, sum(Applied),by = eligibility_id])
  aggr$V1 <- NULL
  for (i in 1:length(top_banks)) {
    my.fun <- function(myData, count) { 
      setDT(myData)[, lapply(.SD, sum), by=eligibility_id, .SDcols=count]
    }
    check <- my.fun(final_offers, top_banks[[i]])
    check$eligibility_id <- NULL
    aggr <- cbind(check,aggr)
  }
  
  
  ## replace with this wherever no of sessions are less than 1% ##
  replace <- sum(final_offers$Applied == 1)/nrow(aggr)
  
  ######################## creating mylist_f and mylist_f2 ###########################
  ## get all combos of top banks
  mylist <- list()
  for (i in 1:length(top_banks)) {
    gen <- t(combn(top_banks, i))
    mylist[[i]] <- (gen)
  }
  
  mylist_f <- list()
  row = 0
  for (i in 1:length(mylist)) {
    for (j in 1:nrow(mylist[[i]])) {
      row <- row + 1
      mylist_f[[row]] <- mylist[[i]][j,]
    }
  }
  
  
  # get all combos where hide banks are not present
  top_banks_new <- setdiff(top_banks,hide_banks)

  mylist <- list()
  for (i in 1:length(top_banks_new)) {
    gen <- t(combn(top_banks_new, i))
    mylist[[i]] <- (gen)
  }
  
  mylist_n <- list()
  row = 0
  for (i in 1:length(mylist)) {
    for (j in 1:nrow(mylist[[i]])) {
      row <- row + 1
      mylist_n[[row]] <- mylist[[i]][j,]
    }
  }
  
  ## subtract above lists
  mylist_f <- setdiff(mylist_f,mylist_n)

  ### combos using hide vector
  mylist <- list()
  
  for (i in 1:length(hide_banks)) {
    gen <- t(combn(hide_banks, i))
    mylist[[i]] <- (gen)
  }
  
  mylist_h <- list()
  row = 0
  for (i in 1:length(mylist)) {
    for (j in 1:nrow(mylist[[i]])) {
      row <- row + 1
      mylist_h[[row]] <- mylist[[i]][j,]
    }
  }
  
  
  #Subtract hide banks
  mylist_f <- setdiff(mylist_f,mylist_h)
  
  
  ## use bigbank cutoff variable
  remove <- list()
  for (i in 1:length(mylist_f)) {
    if (length(mylist_f[[i]]) < bigbank_cutoff + 1) {
      remove[[i]] <- i
    }
  }
  
  if (length(remove) != 0) {
    mylist_f <- mylist_f[-unlist(remove)]
  }
  
  remove <- list()
  for (i in 1:length(mylist_f)) {
    if (length(mylist_f[[i]]) - length(intersect(hide_banks,mylist_f[[i]])) < bigbank_cutoff) {
      remove[[i]] <- i
    }
  }
  
  if (length(remove) != 0) {
    mylist_f <- mylist_f[-unlist(remove)]
  }
  
  mylist_f2 <- mylist_f
  for (i in 1:length(mylist_f)) {
    mylist_f2[[i]] <- setdiff(mylist_f[[i]],hide_banks)
  }
  
  ################### After hiding######################
  start <- aggr
  for (i in 1:length(hide_banks)) {
    start <- start[start[[hide_banks[[i]]]] == 0,]
  }
  
  mapping_a <- list()
  for (i in 1:length(mylist_f2)) {
    check <- start
    for (j in 1:length(mylist_f2[[i]])) {
      s_new2 <- top_banks_new[top_banks_new == mylist_f2[[i]][[j]]]
      check <- check[check[[s_new2]] != 0,]
    }
    
    s_new2 <- setdiff(top_banks_new,mylist_f2[[i]])
    if (length(s_new2 != 0)) {
      for (j in 1:length(s_new2)) {
        check <- check[check[[s_new2[[j]]]] == 0,]
      }
    }
    
    check$gen <- 1
    for (j in 1:length(top_banks)) {
      check[[top_banks[[j]]]] <- NULL
    }
    
    mergeit <- merge(final_offers,check, by = c("eligibility_id"),all.x = T)
    mergeit <- mergeit[!is.na(mergeit$gen),]
    newlist <- list()
    for (j in 1:length(mylist_f2[[i]])) {
      if (nrow(check) > 1*nrow(start)/100) {
        newlist[[j]] <- nrow(mergeit[mergeit$bank_name.a == mylist_f2[[i]][[j]] & !is.na(mergeit$bank_name.a),])/nrow(check)
      }
      else {
        newlist[[j]] <- replace
      }
    }
    mapping_a[[length(mapping_a)+1]] <- newlist
  }
  
  ###################### Before Hiding #####################################
  mapping_b <- list()
  apps_list <- list()
  sessions_list <- list()
  for (i in 1:length(mylist_f)) {
    check <- aggr
    for (j in 1:length(mylist_f[[i]])) {
      s_new2 <- top_banks[top_banks == mylist_f[[i]][[j]]]
      check <- check[check[[s_new2]] != 0,]
    }
    
    s_new2 <- setdiff(top_banks_new,mylist_f[[i]])
    if (length(s_new2 != 0)) {
      for (j in 1:length(s_new2)) {
        check <- check[check[[s_new2[[j]]]] == 0,]
      }
    }
    
    check$gen <- 1
    for (j in 1:length(top_banks)) {
      check[[top_banks[[j]]]] <- NULL
    }
    
    mergeit <- merge(final_offers,check, by = c("eligibility_id"),all.x = T)
    mergeit <- mergeit[!is.na(mergeit$gen),]
    
    newlist <- list()
    newlist2 <- list()
    newlist3 <- list()
    for (j in 1:length(mylist_f[[i]])) {
      if (nrow(check) > 1*nrow(aggr)/100) {
        newlist[[j]] <- nrow(mergeit[mergeit$bank_name.a == mylist_f[[i]][[j]] & !is.na(mergeit$bank_name.a),])/nrow(check)
      }
      else {
        newlist[[j]] <- replace
      }
      newlist2[[j]] <- nrow(mergeit[mergeit$bank_name.a == mylist_f[[i]][[j]] & !is.na(mergeit$bank_name.a),])
      newlist3[[j]] <- nrow(check)
      }
    mapping_b[[length(mapping_b)+1]] <- newlist
    apps_list[[length(apps_list)+1]] <- newlist2
    sessions_list[[length(sessions_list)+1]] <- newlist3
  }
  
  
  #################################Finding app share after hiding######################################################
  ## modifying sessions list
  sessions_list_n <- list()
  for (i in 1:length(sessions_list)) {
    sessions_list_n[[i]] <- max(unlist(sessions_list[[i]]))
  }
  
  finalapps <-  list()
  for (i in 1:length(mylist_f)) {
    newlist4 <- list()
    for (j in 1:length(mylist_f2[[i]])) {
      newlist4[[j]] <- mapping_a[[i]][[j]]*sessions_list_n[[i]]
      newlist4[[j]] <- round(newlist4[[j]])
    }
    finalapps[[length(finalapps)+1]] <- newlist4
  }
  
  ########## bankwise app share #############
  total_apps_b <- sum(unlist(apps_list))
  total_apps_a <- sum(unlist(finalapps))
  
  bankwise <- list()
  for (i in 1:length(top_banks)) {
    newlist5 <- list()
    for (j in 1:length(unlist(mylist_f))) {
      if (top_banks[[i]] == unlist(mylist_f)[[j]]) {
        newlist5[[j]] <- j
      }
    }
    bankwise[[length(bankwise)+1]] <- newlist5
  } 
  
  for (i in 1:length(bankwise)) {
    bankwise[[i]] <- unlist(bankwise[[i]])
  }
  
  apps_before <- list()
  for (i in 1:length(bankwise)) {
    apps_before[[i]] <- sum(unlist(apps_list)[bankwise[[i]]])
  }
  
  ## apps after
  bankwise_a <- list()
  for (i in 1:length(top_banks_new)) {
    newlist6 <- list()
    for (j in 1:length(unlist(mylist_f2))) {
      if (top_banks_new[[i]] == unlist(mylist_f2)[[j]]) {
        newlist6[[j]] <- j
      }
    }
    bankwise_a[[length(bankwise_a)+1]] <- newlist6
  } 
  
  for (i in 1:length(bankwise_a)) {
    bankwise_a[[i]] <- unlist(bankwise_a[[i]])
  }
  
  apps_after <- list()
  for (i in 1:length(bankwise_a)) {
    apps_after[[i]] <- sum(unlist(finalapps)[bankwise_a[[i]]])
  }
  
  
  #######################################################################################
  library(XLConnect, quietly = TRUE)
  ## create workbook
  wb <- loadWorkbook('WBName.xlsx', create = TRUE)
  setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
  
  # Create a CellStyle with blue solid foreground
  CellColor1 <- createCellStyle(wb)
  setFillPattern(CellColor1, fill = XLC$"FILL.SOLID_FOREGROUND")
  setFillForegroundColor(CellColor1, color = XLC$"COLOR.RED")
  # Specify the border for the cell style created above
  setBorder(CellColor1, side = c("all"), type = XLC$"BORDER.THIN",
            color = c(XLC$"COLOR.BLACK"))
  
  
  # Create a CellStyle without colour
  CellColor2 <- createCellStyle(wb)
  setFillPattern(CellColor2, fill = XLC$"FILL.SOLID_FOREGROUND")
  setFillForegroundColor(CellColor2, color = XLC$"COLOR.YELLOW")
  # Specify the border for the cell style created above
  setBorder(CellColor2, side = c("all"), type = XLC$"BORDER.THIN",
            color = c(XLC$"COLOR.BLACK"))
  
  
  createSheet(wb,'sheet')
  
  writeWorksheet(wb,"Top Banks before hiding",'sheet',startRow = 1,startCol = 1, header = F)
  writeWorksheet(wb,"App share",'sheet',startRow = 1,startCol = 2, header = F)
  writeWorksheet(wb,"Top Banks after hiding",'sheet',startRow = 1,startCol = 4, header = F)
  writeWorksheet(wb,"App share",'sheet',startRow = 1,startCol = 5, header = F)
  setCellStyle(wb, sheet = 'sheet', row=1, col=1:2, cellstyle=CellColor1)
  setCellStyle(wb, sheet = 'sheet', row=1, col=4:5, cellstyle=CellColor1)
  
  writeWorksheet(wb,top_banks,'sheet',startRow = 2,startCol = 1, header = F)
  writeWorksheet(wb,unlist(apps_before),'sheet',startRow = 2,startCol = 2, header = F)
  
  for (i in 1:length(top_banks)) {
    setCellStyle(wb, sheet = 'sheet', row=i+1, col=1:2, cellstyle=CellColor2)
  }
  
  writeWorksheet(wb,top_banks_new,'sheet',startRow = 2,startCol = 4, header = F)
  writeWorksheet(wb,unlist(apps_after),'sheet',startRow = 2,startCol = 5, header = F)
  
  for (i in 1:length(top_banks_new)) {
    setCellStyle(wb, sheet = 'sheet', row=i+1, col=4:5, cellstyle=CellColor2)
  }
  
    
  writeWorksheet(wb,"Before Hiding",'sheet',startRow = 5+length(top_banks),startCol = 1, header = F)
  writeWorksheet(wb,"app count",'sheet',startRow = 5+length(top_banks),startCol = 2, header = F)
  writeWorksheet(wb,"no_of_elig's",'sheet',startRow = 5+length(top_banks),startCol = 3, header = F)
  writeWorksheet(wb,"apps/elig's ratio",'sheet',startRow = 5+length(top_banks),startCol = 4, header = F)
  writeWorksheet(wb,"After Hiding",'sheet',startRow = 5+length(top_banks),startCol = 6, header = F)
  writeWorksheet(wb,"apps/elig's ratio historical",'sheet',startRow = 5+length(top_banks),startCol = 7, header = F)
  writeWorksheet(wb,"app count",'sheet',startRow = 5+length(top_banks),startCol = 8, header = F)
  setCellStyle(wb, sheet = 'sheet', row=5+length(top_banks), col=1:4, cellstyle=CellColor1)
  setCellStyle(wb, sheet = 'sheet', row=5+length(top_banks), col=6:8, cellstyle=CellColor1)
  
  gap = 5+length(top_banks)
  for (i in 1:length(mylist_f)) {
    for (j in 1:length(mylist_f[[i]])) {
      writeWorksheet(wb,mylist_f[[i]][[j]],'sheet',startRow = i+gap+j-1,startCol = 1, header = F)
      writeWorksheet(wb,mapping_b[[i]][[j]],'sheet',startRow = i+gap+j-1,startCol = 4, header = F)
      writeWorksheet(wb,sessions_list[[i]][[j]],'sheet',startRow = i+gap+j-1,startCol = 3, header = F)
      writeWorksheet(wb,apps_list[[i]][[j]],'sheet',startRow = i+gap+j-1,startCol = 2, header = F)
    }
    
    for (j in 1:length(mylist_f2[[i]])) {
      writeWorksheet(wb,mylist_f2[[i]][[j]],'sheet',startRow = i+gap+j-1,startCol = 6, header = F)
      writeWorksheet(wb,mapping_a[[i]][[j]],'sheet',startRow = i+gap+j-1,startCol = 7, header = F)
      writeWorksheet(wb,finalapps[[i]][[j]],'sheet',startRow = i+gap+j-1,startCol = 8, header = F)
    }
        
    for (j in 1:length(mylist_f[[i]])) {
      setCellStyle(wb, sheet = 'sheet', row=i+gap+j-1, col=1:4, cellstyle=CellColor2)
      setCellStyle(wb, sheet = 'sheet', row=i+gap+j-1, col=6:8, cellstyle=CellColor2)
    }    
    gap = gap + length(mylist_f[[i]])
  }
  setColumnWidth(wb,'sheet',column = 1:8)
  saveWorkbook(wb, WB_name)
}






