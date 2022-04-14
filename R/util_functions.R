library(RSQLite)
library(DBI)
library(stringr)

#FUNCTIONS -------------
setBooleanProperties <- function(mylist, x, properties) {
  #mylist: [list]
  # will be extended based on properties
  #x: [string]
  # string from the table schema
  #properties: [named list]
  # property_name=string
  # for each value will try to match string in x. If match found property name is set TRUE in mylist
  for (n in names(properties)) {
    string_to_match <- properties[[n]]
    if (str_detect(x,string_to_match)) {mylist[[n]] <- TRUE} else {mylist[[n]] <- FALSE}
  }
  return(mylist)
}

parseTablesSchema <- function(mydb, user_cols, ignore_tables) {
  #mydb: [db connection]
  #user_col: [named list]
  # table_name=col_name
  # for a tables define which column should contain userID (so this can be filled in by default)
  parsedschema <- list()
  tables_schema <- dbGetQuery(mydb, 'SELECT sql FROM sqlite_master WHERE type == "table"')
  message(nrow(tables_schema), " tables in the database")
  for (i in 1:nrow(tables_schema)) {
    sqlschema <- tables_schema[i,1]
    schema_lines <- unlist(strsplit(sqlschema,split = "\n"))
    tableid <- str_match(schema_lines[1], "CREATE TABLE (.+) \\(")[,2]
    tableid <- gsub('"','', tableid)
    if (tableid %in% ignore_tables) {next}
    message("parsing ", tableid)
    parsedschema[[tableid]] <- list(columns=list(),foreignkeys=list(), checks=list())
    for (x in schema_lines[2:length(schema_lines)]) {
      if (str_detect(x, '.*"([^"]+)"[\\s\\t]+(TEXT|INTEGER|REAL|DATETIME|BLOB)')) {
        fields <- str_match(x, '"([^"]+)"[\\s\\t]+(TEXT|INTEGER|REAL|DATETIME|BLOB)')
        colname <- fields[,2]
        coltype <- fields[,3]
        parsedschema[[tableid]]$columns[[colname]]$type <- coltype
        
        if(is.null(user_cols[[tableid]])) {
          parsedschema[[tableid]]$columns[[colname]]$username <- FALSE
        } else {
          if(user_cols[[tableid]] == colname) {
            parsedschema[[tableid]]$columns[[colname]]$username <- TRUE
          } else {
            parsedschema[[tableid]]$columns[[colname]]$username <- FALSE
          }
        }
        
        parsedschema[[tableid]]$columns[[colname]] <- setBooleanProperties(
          parsedschema[[tableid]]$columns[[colname]], x,
          list(unique="UNIQUE",
               not_null="NOT NULL",
               primary="PRIMARY KEY",
               timestamp="DEFAULT CURRENT_TIMESTAMP")
        )
      } else if (str_detect(x, 'FOREIGN KEY[\\s\\t]*\\((.+)\\)[\\s\\t]*REFERENCES[\\s\\t](.+)[\\s\\t]*\\((.+)\\)')) {
        fields <- str_match(x, 'FOREIGN KEY[\\s\\t]*\\((.+)\\)[\\s\\t]*REFERENCES[\\s\\t](.+)[\\s\\t]*\\((.+)\\)')
        colname <- fields[,2]
        foreigntable <- fields[,3]
        foreigncolumn <- fields[,4]
        parsedschema[[tableid]]$foreignkeys[[colname]] <- c(foreigntable, foreigncolumn)
      } else if (str_detect(x, 'CHECK[\\s]*\\((.+) IN \\((.+)\\)\\)')) {
        fields <- str_match(x, 'CHECK[\\s]*\\((.+) IN \\((.+)\\)\\)')
        colname <- fields[,2]
        values <- fields[,3]
        parsedschema[[tableid]]$checks[[colname]] <- gsub("'","",unlist(strsplit(values, split=",")))
      }
    }
  }
  return(parsedschema)
}

parseViews <- function(mydb) {
  views <- c()
  views_schema <- dbGetQuery(mydb, 'SELECT sql FROM sqlite_master WHERE type == "view"')
  message(nrow(views_schema), " views in the database")
  for (i in 1:nrow(views_schema)) {
    sqlschema <- views_schema[i,1]
    schema_lines <- unlist(strsplit(sqlschema,split = "\n"))
    id <- str_match(schema_lines[1], "CREATE VIEW (.+) ")[,2]
    views <- c(views,id)
  }
  return(views)
}

concatCols <- function(columns) {
  if (length(columns) > 0) {
    cols <- paste(columns, collapse=", ")
  } else {
    cols <- '*'
  }
  return(cols)
}

makePlaceholder <- function(n) {
  if(n == 1) {
    placeholder <- '?'
  } else if(n > 1) {
    placeholder <- paste(rep("?", n), collapse=", ")
  }
  return(placeholder)
}

makeQuery <- function(mydb, columns=c(), table, conditions=list(), logic="AND") {
  #mydb: db connection
  #columns: [vector]
  # columns to retrieve
  #table: [string]
  # table name
  #conditions: [named list of list]
  # pairs of col id - list(operator, value)
  # value is expected to be a vector when operator = "IN"
  conditions_statement <- ""
  values <- c()
  cols <- concatCols(columns)
  if (length(conditions) > 0) {
    statements <- c()
    for(col_name in names(conditions)) {
      placeholder <- makePlaceholder(length(conditions[[col_name]][[2]]))
      if (conditions[[col_name]][[1]] == "IN") {
        placeholder = paste0("(",placeholder,")")
      }
      statement <- paste(c(col_name,conditions[[col_name]][[1]],placeholder),collapse=" ")
      statements <- c(statements, statement)
      values <- c(values, conditions[[col_name]][[2]])
    }
    conditions_statement <- paste0("WHERE ",
                                   paste(statements, collapse=paste0(" ",logic," ")))
  }  
  query_statement <- paste0("SELECT ", cols, " FROM ", table,
                            conditions_statement)
  sql <- sqlInterpolate(mydb, query_statement, .dots=values)
  return(sql)
} 

makeInsertStatement <- function(mydb, table_id, col_values=list()) {
  cols <- concatCols(names(col_values))
  values <- unlist(unname(col_values))
  placeholder <- makePlaceholder(length(col_values))
  statement <- paste0("INSERT INTO ", table_id, "(", cols, ") VALUES (", placeholder, ")")
  sql <- sqlInterpolate(mydb, statement, .dots = values)
}

makeBulkInsertStatement <- function(mydb, table_id, df) {
  cols <- colnames(df)
  placeholder <- makePlaceholder(length(cols))
  values <- c()
  for (i in 1:nrow(df)) {
    values <- c(values,
                unlist(unname(df[i,])))
  }
  values_string <- paste(rep(paste0("(", placeholder, ")"),nrow(df)), collapse=",")
  
  statement <- paste0("INSERT INTO ", table_id, 
                      " (", paste(cols, collapse=", ") , ") ", 
                      "VALUES ", values_string)
  
  sql <- sqlInterpolate(mydb, statement, .dots = values)
}

makeUpdateStatement <- function(mydb, table_id, rowid_col, df) {
  row_statements <- c()
  for (i in 1:nrow(df)) {
    set_cols <- c()
    for(col in colnames(df)) {
      if (col != rowid_col) {set_cols <- c(set_cols, paste0(col, " = ", df[1,col]))}
    }
    set_statement <- paste(set_cols, collapse = ", ")
    row_statements <- c(row_statements, 
                        paste0("UPDATE ", table_id, " SET ", set_statement, " WHERE row id = '", df[i, rowid_col], "'"))
  }
  sql <- paste(row_statement, collapse=";")
  return(sql)
}

validateInputField <- function(value, mydb, table, col, col_params) {
  if (col_params$unique) {
    query <- makeQuery(mydb,c(col),table)
    known_values <- dbGetQuery(mydb, query)
    if (value %in% known_values[,1]) {
      return("NOTUNIQUE")
    }
  } 
  if (col_params$not_null) {
    if ((is.null(value) || is.na(value) || value == "") && !col_params$username) {
      return("ISNULL")
    }
  }
  return("OK")
}

makeVecFromList <- function(x) {
  res <- c()
  for(n in names(x)) {
    res <- c(res,paste0(n,": ", paste(x[[n]], collapse=",")))
  }
  return(res)
}

executeStatement <- function(db, sql, operation, msg) {
  if (is.null(sql)) { return (NULL) }
  result <- dbExecute(db, sql)
  if (result > 0) {
    shinyalert(operation, paste0(msg, " - SUCCESS"), type="success")
    return(as.character(sql))
  } else {
    shinyalert(operation, paste0(msg, " - FAILED"), type = "error")
    return(NULL)
  }
}

cleanOldFiles <- function(mydir, ext, max_files) {
  files <- list.files(mydir, ext)
  if (length(files) > max_files) {
    idx_to_remove <- length(files) - max_files
    for(i in idx_to_remove) {
      file.remove(paste0(mydir,"/", files[i]))
    }
  }
}

getWholeTable <- function(mydb, table_id, tables_schema) {
  sql <- sqlInterpolate(mydb, "SELECT * FROM ?", .dots = c(table_id))
  df <- dbGetQuery(mydb, sql)
  
  #Check if there is a primary key in this case rename it rowid and move it as first column
  #If no primary key in the table, then use rowids from SQL table
  primarycol <- NULL
  for(col in names(tables_schema[[table_id]]$columns)) {
    if (tables_schema[[table_id]]$columns[[col]]$primary) { primarycol <- col}
  }
  
  if(is.null(primarycol)) {
    sql <- sqlInterpolate(mydb, "SELECT rowid FROM ?", .dots = c(table_id))
    rowid <- dbGetQuery(mydb, sql)
  } else {
    primarycol_idx <- which(colnames(df) == primarycol)
    rowid <- data.frame(rowid=df[,primarycol])
    df <- df[, -primarycol_idx]
  }
  df <- cbind(rowid,df)
  colnames(df)[1] <- "rowid"
  return(df)
}

validateRecordTable <- function(df, mydb, table_id, tables_schema) {
  col_names <- colnames(df)
  
  #Validate rows
  allvalidate=TRUE
  notunique_rows <- list()
  isnull_rows <- list()
  
  #Check if there are dup values in unique columns
  dup_values <- list()
  for(col in col_names) {
    if(tables_schema[[table_id]]$columns[[col]]$unique) {
      counts <- table(df[,col])
      if (sum(counts > 1)) {
        dup_values[[col]] <- names(counts[counts > 1])
      }
    }
  }
  if(length(dup_values) > 0) {
    msg <- paste0("Duplicated values found in the following columns and not allowed in ", table_id,"\n",
                  paste(makeVecFromList(dup_values), collapse="\n"))
    shinyalert("Input validation", msg, type = "error")
    allvalidate=FALSE
  }
  
  #Validate each rows according to table schema constraints
  for(i in 1:nrow(df)) {
    for(col in col_names) {
      value <- df[i,col]
      switch(validateInputField(value,mydb,table_id,col,tables_schema[[table_id]]$columns[[col]]),
             NOTUNIQUE = {
               notunique_rows[[col]] <- c(notunique_rows[[col]], i)
             },
             ISNULL = {
               isnull_rows[[col]] <- c(isnull_rows[[col]], i)
             },
             OK = {
               next
             })
    }
  }
  
  if(length(notunique_rows) > 0) {
    msg <- paste0("We found values already present in ", table_id," in some rows for the following unique columns\n",
                  paste(makeVecFromList(notunique_rows), collapse="\n"))
    shinyalert("Input validation", msg, type = "error")
    allvalidate = FALSE
  }
  
  if(length(isnull_rows) > 0) {
    msg <- paste0("We found empty values in some rows for the following mandatory columns in ", table_id, "\n",
                  paste(makeVecFromList(notunique_rows), collapse="\n"))
    shinyalert("Input validation", msg, type = "error")
    allvalidate = FALSE
  }
  return(allvalidate)
}

getMappedIDs <- function(mydb, id_col, map_col, table_id) {
#id_col is the column containing the actual values from table_id
#map_col is the col containing values that will be shown to user
#returns a named vector with values = id_col and names = map_col
  query <- paste0("SELECT ", 
                  paste(c(id_col,map_col), collapse=", "), 
                  " FROM ", table_id)
  res <- unique(dbGetQuery(mydb,query))
  if(nrow(res)>0){
    values <- res[,1]
    names(values) <- paste0(res[,2], " (", res[,1], ")")
  } else {
    values = c()
  }
  return(values)
}