
# TODO update the following path, replacing x.x with the version of Bayes Server you are using
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_171')
jarPath <- 'C:/Program Files/Bayes Server/Bayes Server 7.25/API/Java/bayesserver-7.25.jar'

library(rJava) #load the rJava library
.jinit(classpath = jarPath)

Boolean <- J("java.lang.Boolean")

Double <- J("java.lang.Double")
Integer <- J("java.lang.Integer")
Boolean <- J("java.lang.Boolean")
String <- J("java.lang.String")
List <- J("java.util.List")
Collection <- J("java.util.Collection")
AbstractCollection <- J("java.util.AbstractCollection")
AbstractList <- J("java.util.AbstractList")
Enum <- J("java.lang.Enum")

DataTable <- J("com.bayesserver.data.DataTable")
DataColumn <- J("com.bayesserver.data.DataColumn")
DataColumnCollection <- J("com.bayesserver.data.DataColumnCollection")
DataRow <- J("com.bayesserver.data.DataRow")
DataRowCollection <- J("com.bayesserver.data.DataRowCollection")
Network <- J("com.bayesserver.Network")
State <- J("com.bayesserver.State")
StateContext <- J("com.bayesserver.StateContext")
VariableContext <- J("com.bayesserver.VariableContext")
Node <- J("com.bayesserver.Node")
Variable <- J("com.bayesserver.Variable")
Link <- J("com.bayesserver.Link")
VariableValueType <- J("com.bayesserver.VariableValueType")
NetworkVariableCollection <- J("com.bayesserver.NetworkVariableCollection")
QueryDistribution <- J("com.bayesserver.inference.QueryDistribution")
DefaultQueryDistributionCollection <- J("com.bayesserver.inference.DefaultQueryDistributionCollection")
Table <- J("com.bayesserver.Table")
StateValueType <- J("com.bayesserver.StateValueType")
VariableDefinition <- J("com.bayesserver.data.discovery.VariableDefinition")

HeadTail <- J("com.bayesserver.HeadTail")
TemporalType <- J("com.bayesserver.TemporalType")
TimeValueType <- J("com.bayesserver.data.TimeValueType")
Table <- J("com.bayesserver.Table")
CLGaussian <- J("com.bayesserver.CLGaussian")
TableIterator <- J("com.bayesserver.TableIterator")
RelevanceTreeInferenceFactory <- J("com.bayesserver.inference.RelevanceTreeInferenceFactory")
ParameterLearning <- J("com.bayesserver.learning.parameters.ParameterLearning")
ParameterLearningOptions <- J("com.bayesserver.learning.parameters.ParameterLearningOptions")
DatabaseDataReaderCommand <- J("com.bayesserver.data.DatabaseDataReaderCommand")
DataTableDataReaderCommand <- J("com.bayesserver.data.DataTableDataReaderCommand")
ReaderOptions <- J("com.bayesserver.data.ReaderOptions")
TemporalReaderOptions <- J("com.bayesserver.data.TemporalReaderOptions")
VariableReference <- J("com.bayesserver.data.VariableReference")
ColumnValueType <- J("com.bayesserver.data.ColumnValueType")
DefaultEvidenceReaderCommand <- J("com.bayesserver.data.DefaultEvidenceReaderCommand")
DataTable <- J("com.bayesserver.data.DataTable")
ValidationOptions <- J("com.bayesserver.ValidationOptions")
PCStructuralLearning <- J("com.bayesserver.learning.structure.PCStructuralLearning")
PCStructuralLearningOptions <- J("com.bayesserver.learning.structure.PCStructuralLearningOptions")
SearchStructuralLearning <- J("com.bayesserver.learning.structure.SearchStructuralLearning")
SearchStructuralLearningOptions <- J("com.bayesserver.learning.structure.SearchStructuralLearningOptions")
HierarchicalStructuralLearning <- J("com.bayesserver.learning.structure.HierarchicalStructuralLearning")
HierarchicalStructuralLearningOptions <- J("com.bayesserver.learning.structure.HierarchicalStructuralLearningOptions")
ChowLiuStructuralLearning <- J("com.bayesserver.learning.structure.ChowLiuStructuralLearning")
ChowLiuStructuralLearningOptions <- J("com.bayesserver.learning.structure.ChowLiuStructuralLearningOptions")
TANStructuralLearning <- J("com.bayesserver.learning.structure.TANStructuralLearning")
TANStructuralLearningOptions <- J("com.bayesserver.learning.structure.TANStructuralLearningOptions")

VariableGenerator <- J("com.bayesserver.data.discovery.VariableGenerator")
VariableGeneratorOptions <- J("com.bayesserver.data.discovery.VariableGeneratorOptions")
VariableDefinition <- J("com.bayesserver.data.discovery.VariableDefinition")
TimeSeriesMode <- J("com.bayesserver.learning.parameters.TimeSeriesMode")

toStateArray <- function(...) {
    return(.jarray(c(...), contents.class = "com.bayesserver.State"))
}

toStateContextArray <- function(...) {
    return(.jarray(c(...), contents.class = "com.bayesserver.StateContext"))
}

toVariableArray <- function(...) {
    return(.jarray(c(...), contents.class = "com.bayesserver.Variable"))
}

toList <- function(...) {

    xs <- new(J("java.util.ArrayList"))

    lapply(c(...), function(x) { xs$add(x) })


    return(xs)
}

toNodeArray <- function(...) {
    return(.jarray(c(...), contents.class = "com.bayesserver.Node"))
}

toDouble <- function(x) {

    return(if (is.na(x)) NULL else new(Double, x))

}

toDoubleArray <- function(...) {

    return(.jarray(lapply(c(...), function(x) { toDouble(x) }), contents.class = "java.lang.Double"))
}

toVariableReferenceList <- function(...) {

    xs <- new(J("java.util.ArrayList"))

    lapply(..., function(x) { xs$add(x) })


    return(xs)
}

licenseBayesServer <- function(licenseKey) {
    J("com.bayesserver.License")$validate(licenseKey)
}

toDataTable <- function(df) {

    dt <- new(DataTable)

    dfTypes <- sapply(df, typeof)
    dfClasses <- sapply(df, class)
    dfNames <- names(df)

    columnCount <- length(dfNames)

    for (i in 1:columnCount) {
        dt$getColumns()$add(dfNames[i], toJavaClass(dfTypes[i], dfClasses[i]))
    }

    for (r in 1:nrow(df)) {
        values <- lapply(df[r,], function(x) {
            return(toJavaObject(x))
        })

        values <- .jarray(values, contents.class = "java.lang.Object")
        dt$getRows()$add(values)
    }

    return(dt)

}

toJavaClass <- function(rType, rClass) {

    if (rType == "double" && rClass == "numeric") {
        return(Double$class)
    }
    else if (rType == "logical" && rClass == "logical") {
        return(Boolean$class)
    }
    else if (rType == "integer" && rClass == "factor") {
        return(String$class) # store factor integer values as factor level names
    }
    else if (rType == "integer" && rClass == "integer") {
        return(Integer$class)
    }
    else if (rType == "character") {
        return(String$class)
    }
    else {
        stop(sprintf("type [%s] or class [%s] not supported", rType, rClass))
    }

}

toJavaObject <- function(x) {

    rType <- typeof(x)
    rClass <- class(x)

    if (rType == "double" && rClass == "numeric") {
        return(new(Double, x))
    }
    else if (rType == "logical" && rClass == "logical") {
        return(new(Boolean, x))
    }
    else if (rType == "integer" && rClass == "integer") {
        return(new(Integer, x))
    }
    else if (rType == "integer" && rClass == "factor") {
        s <- as.character(x)

        return(new(String, s))
    }
    else if (rType == "character") {
        return(new(String, as.character(x)))
    }
    else {
        stop(sprintf("type [%s] or class [%s] not supported", rType, rClass))
    }

}

