# source("./bayesserver.R")

# batch train
trainNetwork <- function(df, structuremethod) {
    sprintf("train bayes server network for %d * %d data with %s\n", nrow(df), ncol(df), structuremethod)
    bn <- prepareBayesServer(df)
    bn$network <- trainStructure(bn$network, bn$dt, structuremethod)
    bn$network <- trainParam(bn$network, bn$dt)
    return(bn$network)
}

# prepare network and datatable
prepareBayesServer <- function(df) {
    sprintf("create empty network for %d * %d data \n", nrow(df), ncol(df))
    dfs <- cleanStringColumn(df)
    network <- createNetworkNoLinks(dfs)
    cat(sprintf("    build dataTable object for %d * %d data\n", nrow(df), ncol(df)))
    dt <- toDataTable(dfs)
    cat(sprintf("    done \n"))
    return(list(network = network, dt = dt))
}


# structure learning
trainStructure <- function(network, dt, method) {
    cat(sprintf("    train structure %d * %d data\n", nrow(df), ncol(df)))
    # structure learning
    switch(method,
           "pc" = { learning = new(PCStructuralLearning); options <- new(PCStructuralLearningOptions) },
           "search" = { learning = new(SearchStructuralLearning); options <- new(SearchStructuralLearningOptions) },
           "hierarchical" = { learning = new(HierarchicalStructuralLearning); options <- new(HierarchicalStructuralLearningOptions) },
           "chow-liu" = { learning = new(ChowLiuStructuralLearning); options <- new(ChowLiuStructuralLearningOptions) },
           "tan" = { learning = new(TANStructuralLearning); options <- new(TANStructuralLearningOptions) },
           { learning = new(PCStructuralLearning); options <- new(PCStructuralLearningOptions) }
    )
    dataReaderCommand <- new(DataTableDataReaderCommand, dt)

    variableReferences <- getVariableReference(network)
    evidenceReaderCommand <- new(
      DefaultEvidenceReaderCommand,
      dataReaderCommand,
      toVariableReferenceList(variableReferences),
      new(ReaderOptions)
    )

    output <- learning$learn(evidenceReaderCommand, network$getNodes(), options)
    return(network)
}

# parameter learning
trainParam <- function(network, dt) {
  cat(sprintf("    train parameter %d * %d data\n", nrow(df), ncol(df)))
    learning <- new(ParameterLearning, network, new(RelevanceTreeInferenceFactory))
    options <- new(ParameterLearningOptions)
    dataReaderCommand <- new(DataTableDataReaderCommand, dt)

    variableReferences <- getVariableReference(network)
    evidenceReaderCommand <- new(
                              DefaultEvidenceReaderCommand,
                              dataReaderCommand,
                              toVariableReferenceList(variableReferences),
                              new(ReaderOptions))
    output <- learning$learn(evidenceReaderCommand, options)
    # print(output$getIterationCount())
    return(network)
}



# inference for data frame
inferNetwork <- function(network, df) {
    cat(sprintf("    infer network with evidence %d * %d data\n", nrow(df), ncol(df)))
    factory <- new(RelevanceTreeInferenceFactory)
    inference <- factory$createInferenceEngine(network)
    variables <- network$getVariables()
    varnames <- lapply(variables, function(v) {
        v$getName()
    })

    df <- cleanStringColumn(df)
    cnames <- colnames(df)

    ## construct query
    queryOptions <- factory$createQueryOptions()
    queryDistributions <- inference$getQueryDistributions()
    queryOutput <- factory$createQueryOutput()

    # build query objects for all variables

    variableReferences <- getVariableReference(network)

    targetQueries = lapply(variables, function(v) {
        if (v$getValueType() == VariableValueType$DISCRETE) {
            targetQuery <- new(Table, v)
        } else if (v$getValueType() == VariableValueType$CONTINUOUS) {
            targetQuery <- new(CLGaussian, v)
        }
        queryDistributions$add(targetQuery)
        targetQuery
    })
    
    # tablesList <- getQueryTables(targetQueries)
    # qdc <- as.list(queryDistributions)

    results <- data.frame()
    colvar <- getColumnVarTable(df,variables)
    # infer batch
    for (r in 1:nrow(df)) {
        evidence <- inference$getEvidence()
        setEvidencesByDataFrame(df[r,], evidence, colvar)
        inference$query(queryOptions, queryOutput)

        probs <- lapply(targetQueries, function(c) {
            if (c$getClass()$toString() == "class com.bayesserver.Table") {
                p <- c$getTable()$get(1L)
            } else if (c$getClass()$toString() == "class com.bayesserver.CLGaussian") {
                p <- c$getMean(0L, 0L)
            }
            p
        })

        results <- rbind(results, probs)
        evidence$clear();
    }
    colnames(results) <- varnames
    return(results)

}

# make accelerator cache of table list of targetQueries 
getQueryTables = function(targetQueries) {
  tables <- lapply(targetQueries, function(c) {
    c$getTable()
  })
  return(tables)
}
# make accelerator cache of variable contents ordering columns of df 
getColumnVarTable = function(df,variables) {
  cnames <- colnames(df)
  variablesList <- c()
  statesList <- c()
  statesTypeList <- c()
  for( n in cnames) {
    v <- variables$get(n, TRUE)
    variablesList = c(variablesList , v)
    statesTypeList = c(statesTypeList , v$getStateValueType())
    statesList = c(statesList, v$getStates())
  }
  return(list(variablesList = variablesList , statesList = statesList,statesTypeList = statesTypeList ))
}
# set evidence for row
setEvidencesByDataFrame = function(row, evidence, colvar) {
    for( c in 1:length(row)) {
        value <- row[[c]]
        if (is.null(value) || is.na(value)) next
        v <- colvar$variablesList[[c]]
        if (v$getValueType() == VariableValueType$DISCRETE) {
            stype <- colvar$statesTypeList[[c]]
            states <- colvar$statesList[[c]]
            if (is.factor(value)) {
                value <- as.character(value)
            }
            if (stype == StateValueType$BOOLEAN) {
                s <- states$get(as.character(value))
                evidence$setState(s)
            } else if (stype == StateValueType$NONE) {
                s <- states$get(value)
                evidence$setState(s)
            }
        } else if (v$getValueType() == VariableValueType$CONTINUOUS) {
            evidence$set(v,new(Double, value))
        }
    }
    return(evidence)
}


getVariableReference = function(network) {
    variableReferences = lapply(network$getVariables(), function(v) {
        isDiscrete <- v$getValueType() == VariableValueType$DISCRETE
        isBoolean <- v$getStateValueType() == StateValueType$BOOLEAN
        columnValueType <- if (isDiscrete && !isBoolean) ColumnValueType$NAME else ColumnValueType$VALUE # ifelse not working
        return(new(VariableReference, v, columnValueType, v$getName()))
    })
    return(variableReferences)
}

# convert string columns to factor
cleanStringColumn <- function(df) {
    df <- mutate_if(df, is.character, as.factor)
    return(df)
}

createNetworkNoLinks <- function(df) {
    network <- new(Network)
    cnames <- colnames(df)
    for (name in cnames) {
        sample <- df[1, name]
        if (is.logical(sample)) {
            var1 <- new(Variable, name, c("FALSE", "TRUE"))
            var1$setStateValueType(StateValueType$BOOLEAN)
            node = new(Node, var1)
        } else if (is.numeric(sample)) {
            node = new(Node, name, VariableValueType$CONTINUOUS)
        } else if (is.character(sample)) {
            node = new(Node, name, 1L)
        } else if (is.factor(sample)) {
            if (length(levels(df[, name])) > 1) {
                var1 <- new(Variable, name, levels(df[, name]))
                var1$setStateValueType(StateValueType$NONE)
                node = new(Node, var1)
            } else {
                next
            }
        }
        network$getNodes()$add(node)
    }
    return(network)
}

verboseNetwork <- function(network) {
    lapply(network$getVariables(), function(v) {
        paste0(
            sprintf("variable %d name: %s kind: %s state type: %s", v$getIndex(), v$getName(), v$getKind()$toString(), v$getStateValueType()$toString()),
            lapply(v$getStates(), function(s) {
                sprintf("state %s -> %s", s$getName(), s$getValue())
            })
        )
    })
}

verboseDataTable <- function(dt) {
    print(sprintf("columns num %d", dt$getColumns()$size()))
    print(lapply(dt$getColumns(), function(v) { sprintf("column %s type: %s", v$getColumnName(), v$getDataType()$toString()) }))
    print(sprintf("rows num %d", dt$getRows()$size()))
}