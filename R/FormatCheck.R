# FormatCheck.R is central within the CPO framework. It checks that
# incoming & outgoing data conforms with the properties declared by
# a CPO and with requirements implicit within the CPO framework
# (Task column not changed in Feature Operating CPOs, number of rows
# not changed unless Training Only CPO, data for retrafo is the same
# as data for trafo).
# In the same stride, data format is changed to match the required
# format of the CPO functions -- see `dataformat` argument of
# makeCPO.


##################################
### Externals                  ###
##################################

# do the preparation before calling trafo:
#  - check the data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - split the data
#  - get a shape info object
# @param indata [Task | data.frame] incoming data to be fed to the CPO trafo function
# @param datasplit [character(1)] one of 'task', 'no', 'most', 'all', 'factor', 'onlyfactor', 'numeric', 'ordered'
# @param allowed.properties [character] allowed properties of `indata`
# @param subset.selector [list] information about 'affect.*' parameters that determine which subset of 'indata' is affected
# @param capture.factors [logical(1)] whether to save factor levels of input data in shapeinfo data structure. This is only used if the CPO has 'fix.factors' set to TRUE
# @return [list] the data to feed to the CPO trafo function, as well as meta-information: list(indata = list(data, target), shapeinfo, properties)
prepareTrafoInput = function(indata, datasplit, allowed.properties, subset.selector, capture.factors, name) {
  assert(checkClass(indata, "data.frame"), checkClass(indata, "Task"))
  if ("Task" %in% class(indata)) {
    targets = getTaskTargetNames(indata)
    subset.selector$data = getTaskData(indata, target.extra = TRUE)$data
    subset.index = do.call(getColIndices, subset.selector)
    # subsetTask, but keep everything in order
    indata.data = getTaskData(indata)
    fullindices = seq_along(indata.data)
    aretargets = names(indata.data) %in% getTaskTargetNames(indata)
    new.subset.index = fullindices[!aretargets][subset.index]
    if (all(new.subset.index == sort(new.subset.index))) {
      new.subset.index = sort(c(which(aretargets), new.subset.index))
    } else {
      new.subset.index = c(which(aretargets), new.subset.index)
    }
    indata = changeData(indata, indata.data[new.subset.index])
  } else {
    targets = character(0)
    subset.selector$data = indata
    subset.index = do.call(getColIndices, subset.selector)
    indata = indata[subset.index]
  }

  present.properties = getTaskProperties(indata)
  assertPropertiesOk(present.properties, allowed.properties, "trafo", "in", name)

  shapeinfo = makeInputShapeInfo(indata, capture.factors)
  shapeinfo$subset.selector = subset.selector

  indata = if (is.data.frame(indata)) {
    splitdf(indata, getLLDatasplit(datasplit))
  } else {
    splittask(indata, getLLDatasplit(datasplit))
  }

  tempdata = indata$data
  indata$data = getIndata(indata$data, datasplit)
  list(indata = indata, shapeinfo = shapeinfo, properties = present.properties, tempdata = tempdata, subset.index = subset.index)
}


# do the preparation before calling retrafo:
#  - check data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  - split the data
#  --> return
# how does mlr predict handle this stuff? they just drop target columns by name
# @param indata [Task | data.frame] incoming data to be fed to the CPO retrafo function
# @param datasplit [character(1)] one of 'task', 'no', 'most', 'all', 'factor', 'onlyfactor', 'numeric', 'ordered'
# @param allowed.properties [character] allowed properties of `indata`
# @param shapeinfo.input [InputShapeInfo] information about the data shape used to train the CPO
# @param name [character(1)] name of the CPO to print for debug information
# @return [list] the data to feed to the CPO retrafo function, as well as meta-information:
#   list(indata = data in a shape fit to be fed into retrafo, properties, tempdata, subset.index)
prepareRetrafoInput = function(indata, datasplit, allowed.properties, shapeinfo.input, name) {

  prepared = prepareRetrafoData(indata, datasplit, allowed.properties, shapeinfo.input, name)
  indata = prepared$data

  lldatasplit = getLLDatasplit(datasplit)


  if (lldatasplit %in% c("most", "all")) {
    splitinto = c("numeric", "factor", "other", if (lldatasplit == "all") "ordered")
    indata = splitColsByType(splitinto, indata)
  }

  list(indata = getIndata(indata, datasplit), properties = prepared$properties, tempdata = indata, subset.index = prepared$subset.index)
}

# do the check of the trafo's return value
#  - check the data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check properties are allowed
#  - get a shape info object
# @param outdata [Task | data.frame | matrix] data returned by CPO trafo function
# @param olddata [Task | data.frame] incoming data that was already given to prepareTrafoInput as 'indata'
# @param tempdata [Task | data.frame] incoming data that was given to prepareTrafoInput as 'indata', after subsetting according to 'affect.*' parameters
# @param datasplit [character(1)] one of 'task', 'no', 'most', 'all', 'factor', 'onlyfactor', 'numeric', 'ordered'
# @param allowed.properties [character] allowed properties of `outdata`. That is the union of the CPO's 'properties.needed' field and the properties already present in 'indata'
# @param properties.adding [character] which properties are supposed to be 'added' to the subsequent data handler. Therefore, these properties must be *absent* from `outdata`.
# @param operating.type [character(1)] one of 'target', 'feature', 'retrafoless': whether target data, feature data, or both (but only during trafo) may be changed
# @param convertto [character(1)] only if `operating.type` is 'target': type of the new task
# @param subset.index [numeric] index into olddata columns: the columns actually selected by 'affect.*' parameters
# @param name [character(1)] name of the CPO to print for debug information
# @return [list] the data resulting from the CPO operation, as well as meta-information: list(outdata, shapeinfo)
handleTrafoOutput = function(outdata, olddata, tempdata, datasplit, allowed.properties, properties.adding, operating.type, convertto, subset.index, name) {
  outdata = rebuildOutdata(outdata, tempdata, datasplit)
  datasplit = getLLDatasplit(datasplit)
  if (operating.type == "target") {
    recombined = recombinetask(olddata, outdata, datasplit, subset.index, TRUE, convertto, name)
    small.recombined = recombinetask(subsetTask(olddata, features = subset.index), outdata, datasplit, seq_along(subset.index), TRUE, convertto, name)
  } else if (operating.type == "feature") {
    if (is.data.frame(olddata)) {
      recombined = recombinedf(olddata, outdata, datasplit, subset.index, character(0), name)
      small.recombined = recombinedf(olddata[subset.index], outdata, datasplit, seq_along(subset.index), character(0), name)

    } else {
      recombined = recombinetask(olddata, outdata, datasplit, subset.index, FALSE, name = name)
      small.recombined = recombinetask(subsetTask(olddata, features = subset.index), outdata, datasplit, seq_along(subset.index), FALSE, name = name)
    }
  } else {  # operating.type == "retrafoless"
    recombined = recombineRetrafolessResult(olddata, outdata, datasplit, subset.index, name)
    small.recombined = recombined
  }

  present.properties = getTaskProperties(small.recombined)
  assertPropertiesOk(present.properties, allowed.properties, "trafo", "out", name)
  assertPropertiesOk(present.properties, setdiff(allowed.properties, properties.adding), "trafo", "adding", name)
  if (datasplit %in% c("df.all", "task")) {
    # in this case, take shape info with 'target' separated
    shapeinfo = makeOutputShapeInfo(small.recombined)
  } else {
    shapeinfo = makeOutputShapeInfo(outdata)
  }
  if ("Task" %in% class(olddata)) {
    shapeinfo$target = getTaskTargetNames(olddata)
  }

  list(outdata = recombined, shapeinfo = shapeinfo)
}

# do the check of the retrafo's return value
#  - check data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
# @param outdata [Task | data.frame | matrix] data returned by CPO retrafo function
# @param olddata [Task | data.frame] incoming data that was already given to prepareRetrafoInput as 'indata'
# @param tempdata [Task | data.frame] incoming data that was given to prepareRetrafoInput as 'indata', after subsetting according to 'affect.*' parameters
# @param datasplit [character(1)] one of 'task', 'no', 'most', 'all', 'factor', 'onlyfactor', 'numeric', 'ordered'
# @param allowed.properties [character] allowed properties of `outdata`. That is the union of the CPO's 'properties.needed' field and the properties already present in 'indata'
# @param properties.adding [character] which properties are supposed to be 'added' to the subsequent data handler. Therefore, these properties must be *absent* from `outdata`.
# @param shapeinfo.output [OutputShapeInfo] ShapeInfo describing the shape of the data returned by the CPO `trafo` function when it was called.
#        This imposes the same structure on the retrafo return value.
# @param subset.index [numeric] index into olddata columns: the columns actually selected by 'affect.*' parameters
# @param name [character(1)] name of the CPO to print for debug information
# @return [list] the data resulting from the CPO retrafo operation
handleRetrafoOutput = function(outdata, olddata, tempdata, datasplit, allowed.properties, properties.adding, shapeinfo.output, subset.index, name) {
  outdata = rebuildOutdata(outdata, tempdata, datasplit)
  datasplit = getLLDatasplit(datasplit)
  if (datasplit %in% c("df.all", "task")) {
    # target is always split off during retrafo
    datasplit = "df.features"
  }
  targetcols = character(0)

  if (is.data.frame(olddata)) {
    if (any(shapeinfo.output$target %in% names(olddata))) {
      assert(all(shapeinfo.output$target %in% names(olddata)))  # we also check this in prepareRetrafoInput
      targetcols = shapeinfo.output$target
    }

    recombined = recombinedf(olddata, outdata, datasplit, subset.index, targetcols, name)
    small.recombined = recombinedf(cbind(dropNamed(olddata, targetcols)[subset.index], olddata[targetcols]), outdata, datasplit,
      seq_along(subset.index), targetcols, name)
  } else {
    recombined = recombinetask(olddata, outdata, datasplit, subset.index, FALSE, name = name)
    small.recombined = recombinetask(subsetTask(olddata, features = subset.index), outdata, datasplit, seq_along(subset.index), FALSE, name = name)
  }

  present.properties = getDataProperties(small.recombined, targetcols)
  assertPropertiesOk(present.properties, allowed.properties, "retrafo", "out", name)
  assertPropertiesOk(present.properties, setdiff(allowed.properties, properties.adding), "retrafo", "adding", name)

  # check the shape of outdata is as expected
  shapeinfo.output$target = NULL
  if (datasplit %in% c("all", "most")) {
    assertSetEqual(names(outdata), names(shapeinfo.output))
    for (n in names(outdata)) {
      assertShapeConform(outdata[[n]], shapeinfo.output[[n]], datasplit == "all", name)
    }
  } else {
    assertShapeConform(outdata, shapeinfo.output, FALSE, name)
  }

  recombined
}


# this is a subset of retrafo preparation which also needs to happen for target retrafo
# data prep.
# - split data into 'data' and 'target' DFs (latter possibly empty df)
# - test shapeinfo.input conformity
# @param data [Task | data.frame] incoming data to be prepared (i.e. split and formatted)
# @param datasplit [character(1)] one of 'task', 'no', 'most', 'all', 'factor', 'onlyfactor', 'numeric', 'ordered'
# @param allowed.properties [character] allowed properties of `indata`
# @param shapeinfo.input [InputShapeInfo] information about the data shape used to train the CPO
# @param name [character(1)] name of the CPO to print for debug information
# @return [list] the prepared data, as well as meta-information: list(data = data in a shape fit to be fed into retrafo, properties, subset.index)
prepareRetrafoData = function(data, datasplit, allowed.properties, shapeinfo.input, name) {

  # check that input column names and general types match (num / fac, or num/fac/ordered if datasplit == "all"
  if ("Task" %in% class(data)) {
    if (length(shapeinfo.input$target) && length(getTaskTargetNames(data))) {
      # FIXME: this might be too strict: maybe the user wants to retrafo a Task with the target having a different name?
      # HOWEVER, then either the training data's task didnt matter (and he should have trained with a data.set?), or it
      #  DID matter, in which case it is probably important to have the same data type <==> target name
      assertSetEqual(getTaskTargetNames(data), shapeinfo.input$target, .var.name = sprintf("Target names of Task %s", getTaskId(data)))
    }
    target = getTaskData(data, features = character(0))
    data = getTaskData(data, target.extra = TRUE)$data
  } else {
    if (any(shapeinfo.input$target %in% names(data))) {
      if (!all(shapeinfo.input$target %in% names(data))) {
        badcols = intersect(shapeinfo.input$target, names(data))
        stopf("Some, but not all target columns of training data found in new data. This is probably an error.\n%s%s: %s",
          "Offending column", ifelse(length(badcols) > 1, "s", ""), collapse(badcols, sep = ", "))
      }
      target = data[shapeinfo.input$target]
      data = dropNamed(data, shapeinfo.input$target)
    } else {
      target = data[character(0)]
      shapeinfo.input$target = NULL
    }
  }
  if (!is.data.frame(data)) {
    stopf("Data fed into CPO %s retrafo is not a Task or data.frame.", name)
  }

  shapeinfo.input$subset.selector$data = data
  subset.index = do.call(getColIndices, shapeinfo.input$subset.selector)
  data = data[subset.index]
  shapeinfo.input$subset.selector = NULL

  lldatasplit = getLLDatasplit(datasplit)

  assertShapeConform(data, shapeinfo.input, lldatasplit == "all", name)

  if ("factor.levels" %in% names(shapeinfo.input)) {
    data = fixFactors(data, shapeinfo.input$factor.levels)
  }

  present.properties = getDataProperties(data, character(0))
  assertPropertiesOk(present.properties, allowed.properties, "retrafo", "in", name)

  list(data = data, target = target, properties = present.properties, subset.index = subset.index)
}

# make sure that the factor levels of data.frame 'data' are as described by 'levels'.
# @param data [data.frame] data to check / modify
# @param levels [list of character] levels of `data` columns, indexed by `data` column names
# @return [data.frame] the modified `data`
fixFactors = function(data, levels) {
  assertSubset(names(levels), names(data))
  data[names(levels)] = mapply(factor, data[names(levels)], levels, SIMPLIFY = FALSE)
  data
}


##################################
### Shape & Properties         ###
##################################

# calculate the properties of the data (only feature types & missings)
# data can be a task or data.frame
# @param data [data.frame | Task] the data to check
# @param targetnames [character] only if `data` is a data.frame: the target columns, which will be ignored
# @return [character] a subset of c("numerics", "factors", "ordered", "missings")
getDataProperties = function(data, targetnames) {
  if (is.data.frame(data)) {
    td = makeTaskDescInternal(NULL, NULL, data, targetnames, NULL, NULL)
  } else {
    assertClass(data, "Task")
    td = getTaskDesc(data)
  }
  nf = td$n.feat
  c(names(nf)[nf > 0], if (td$has.missings) "missings")
}

# calculate the properties of the data, as if it were a task.
# If data is a data.frame, we give it the property 'cluster'
# otherwise, we give it the propertye of the task type. If
# applicable, we also set oneclass, multiclass, etc (any from
# the variable 'cpo.targetproperties')
# @param data [data.frame | Task] the data to check
# @return [character] a subset of c("numerics", "factors", "ordered", "missings", "cluster", "classif", "multilabel", "regr", "surv", "oneclass", "twoclass", "multiclass")
getTaskProperties = function(data) {
  if ("Task" %in% data) {
    targetnames = getTaskTargetNames(data)
  } else {
    targetnames = character(0)
  }
  props = getDataProperties(data, targetnames)
  if (is.data.frame(data)) {
    c(props, "cluster")
  } else {
    td = getTaskDesc(data)
    if (td$type == "classif") {
      others = switch(as.character(length(td$class.levels)),
        `1` = "oneclass", `2` = "twoclass", "multiclass")
    } else {
      others = NULL
    }
    c(props, td$type, others)
  }
}

# calculate the properties, properties.adding and properties.needed for a composite CPO
# CPO1 %>>% CPO2
# If the properties don't match (e.g. CPO1 'needs' a property that CPO2 does not support), an error is thrown.
# @param prop1 [list of character] properties of CPO1, with slots "properties", "properties.adding", and "properties.needed"
# @param prop2 [list of character] properties of CPO2, with same slots as `prop1`
# @name1 [character(1)] name of CPO1 to print in debug messages
# @name2 [character(1)] name of CPO2 to print in debug messages
# @return [list of character] properties of compound CPO, with same slots as `prop1`
# returns a list(properties, properties.adding, properties.needed)
compositeProperties = function(prop1, prop2, name1, name2) {
  properties.1 = prop1$properties
  properties.adding.1 = prop1$properties.adding
  properties.needed.1 = prop1$properties.needed
  properties.2 = prop2$properties
  properties.adding.2 = prop2$properties.adding
  properties.needed.2 = prop2$properties.needed
  assertCharacter(properties.1, unique = TRUE)
  assertCharacter(properties.2, unique = TRUE)
  assertCharacter(properties.adding.1, unique = TRUE)
  assertCharacter(properties.adding.2, unique = TRUE)
  assertCharacter(properties.needed.1, unique = TRUE)
  assertCharacter(properties.needed.2, unique = TRUE)
  assertString(name1)
  assertString(name2)
  # some explanation about properties:
  # * 'properties' are the properties that a CPO can handle.
  # * 'properties.adding' are the properties it adds to the things coming after it, it is therefore
  #   the things it *removes* from a dataset. E.g. if it removes 'missings' from data, it adds the property
  #   'missings' to the pipeline.
  # * properties.needed are the properties it needs from the things coming after it, this are the
  #   the things it *adds* to a dataset. E.g. if it converts numerics to factors, it 'needs' the learner /
  #   CPOs coming after it to have the property 'factors'.

  # The conditions on the properties are:
  # A) properties.adding is a subset of properties
  # B) properties.adding and properties.needed have no common elements
  # (these should be checked upon creation of a CPO)

  # When composing two CPOs (CPO1 %>>% CPO2), there is an additional requirement:
  # * properties.needed.1 is a subset of properties.2
  missing.properties = setdiff(properties.needed.1, properties.2)
  if (isPropertyStrict() && length(missing.properties)) {
    stopf("CPO %s creates data with propert%s %s that %s can not handle.",
      name1, ifelse(length(missing.properties) > 1, "ies", "y"),
      collapse(missing.properties, sep = ", "),
      name2)
  }

  # The properties of the new CPO are obtained thus:
  properties.composite = intersect(properties.1, union(properties.2, properties.adding.1))
  properties.adding.composite = union(setdiff(properties.adding.1, properties.needed.2), intersect(properties.1, properties.adding.2))
  properties.needed.composite = union(setdiff(properties.needed.1, properties.adding.2), properties.needed.2)

  # Proofs that conditions (A) and (B) are still fulfilled:
  # A) using distribution of union and intersect, and the fact that (cond (A)) intersect(properties.adding.1, properties.1) == properties.adding.1,
  #    we rewrite
  #      properties.composite = union(properties.adding.1, intersect(properties.1, properties.2))
  #    Now the first term in the union of properties.adding.composite is a subset of the first term of the union of properties.composite;
  #    same with the second term of both.
  # B) We show that intersect(properties.adding.composite, properties.needed.composite) is empty by showing that both terms in the union
  #    of properties.adding.composite have empty intersect each with both terms in the union of properties.needed.composite.
  #    1) intersect(properties.adding.1 - properties.needed.2, properties.needed.2) is empty because properties.needed.2 is subtracted from the lhs
  #    2) intersect(properties.adding.1 - properties.needed.2, properties.needed.1 - properties.adding.2) is empty because
  #       intersect(properties.adding.1, properties.needed.1) is empty per condition (B)
  #    3) intersect(intersect(properties.1, properties.adding.2), properties.needed.1 - properties.adding.2) is empty because properties.adding.2
  #       is subtracted from the rhs
  #    4) intersect(intersect(properties.1, properties.adding.2), properties.needed.2) is empty because properties.needed.2 and properties.adding.2
  #       have empty intersect per condition (B)
  list(properties = properties.composite, properties.adding = properties.adding.composite, properties.needed = properties.needed.composite)
}

# give error when shape is different than dictated by shapeinfo.
# wasresult: whether we are checking the result of retrafo
# @param df [data.frame] the data to check
# @param shapeinfo [ShapeInfo] a the shape which `df` must conform to
# @param checkordered [logical(1)] whether to check for 'ordered' as a type differing from 'factor'
# @param name [character(1)] name of the CPO currently being run, for error and debug printing
# @return [invisible(NULL)]
assertShapeConform = function(df, shapeinfo, checkordered, name) {
  if (!identical(names2(df), shapeinfo$colnames)) {
    stopf("Error in CPO %s: column name mismatch between training and test data.\nWas %s during training, is %s now.",
          name, collapse(shapeinfo$colnames, sep = ", "), collapse(names(df), sep = ", "))
  }
  indata = df[shapeinfo$colnames]

  if (checkordered) {
    typesmatch = list(
        c("integer", "numeric"),
        "factor", "ordered")
  } else {
    typesmatch = list(
        c("integer", "numeric"),
        c("factor", "ordered"))
  }

  newcoltypes = vcapply(indata, function(x) class(x)[1])

  for (t in typesmatch) {
    typemismatch = (newcoltypes %in% t) != (shapeinfo$coltypes %in% t)
    if (any(typemismatch)) {
      plurs = ifelse(sum(typemismatch) > 1, "s", "")
      singes = ifelse(sum(typemismatch) > 1, "", "es")
      stopf("Error in CPO %s: Type%s of column%s %s mismatch%s between training and test data.", name,
        plurs, plurs, collapse(names(indata)[typemismatch], sep = ", "), singes)
    }
  }
}

# prepare some information about the data shape, so retrafo can check that
# it gets the kind of data it expects
# this needs to be checked both for input and for output
# @param data [data.frame] the data for which the shape is to be created
# @return [ShapeInfo] a simple datastructure that contains information about data column names and types
makeShapeInfo = function(data) {
  makeS3Obj("ShapeInfo",
    colnames = colnames(data),
    coltypes = vcapply(data, function(x) class(x)[1]))
}

# like makeShapeInfo, but additionally get the target names and possibly factor levels
# @param indata [data.frame | Task] data for which the shape is to be created
# @param capture.factors [logical(1)] whether to capture factor levels
# @return [InputShapeInfo] a datastructure extending `ShapeInfo` containing information about the data shape
makeInputShapeInfo = function(indata, capture.factors) {
  if ("Task" %in% class(indata)) {
    target = getTaskTargetNames(indata)
    indata = getTaskData(indata, target.extra = TRUE)$data
    ret = makeShapeInfo(indata)
    ret$target = target
  } else {
    ret = makeShapeInfo(indata)
  }
  if (capture.factors) {
    ret$factor.levels = Filter(function(x) !is.null(x), lapply(indata, levels))
  }
  addClasses(ret, "InputShapeInfo")
}

# creates shape info for data coming out of trafo, so retrafo can check that the data generated
# by it conforms to the data returned by trafo earlier.
# This does not do certain tests about the form of `outdata`, so it is recommended to call this
# after `recombinetask` was called (but with the *original* data, not the recombined data).
# @param outdata [data.frame | Task | list of data.frame] data returned by `trafo` function of which the shape is to be covered
# @return [OutputShapeInfo] This either extends `ShapeInfo` (if outdata is `data.frame` or `Task`) or is a list of `ShapeInfo` objects.
makeOutputShapeInfo = function(outdata) {
  if (is.data.frame(outdata)) {
    res = makeShapeInfo(outdata)
  } else if ("Task" %in% class(outdata)) {
    res = makeShapeInfo(getTaskData(outdata, target.extra = TRUE)$data)
  } else {
    # data is split by type, so we get the shape of each of the constituents
    res = lapply(outdata, makeShapeInfo)
  }
  addClasses(res, "OutputShapeInfo")
}

# give userfriendly error message when data does have the properties it is allowed to have.
# @param present.properties [character] properties that were found in a given data object
# @param allowed.properties [character] the properties that the data object is allowed to have
# @param whichfun [character(1)] name of the CPO
# @param direction [character(1)] either "in" (data is being sent into CPO), "out" (data was returned by CPO function, some
#   properties are present that were *not* present in the input data, but the given properties were not declared as
#   'properties.needed'), or "adding" (data was returned by CPO function, but the given properties *were* declared as
#   'properties.adding' and hence must not be present)
# @return [invisible(NULL)]
assertPropertiesOk = function(present.properties, allowed.properties, whichfun, direction, name) {
  if (!isPropertyStrict()) {
    return(invisible(NULL))
  }
  badprops = setdiff(present.properties, allowed.properties)
  if (length(badprops)) {
    if (direction == "in") {
      stopf("Data going into CPO %s has propert%s %s that %s can not handle.",
        whichfun, ifelse(length(badprops) > 1, "ies", "y"),
        collapse(badprops, sep = ", "), name)
    } else if (direction == "out") {
      stopf("Data returned by CPO %s has propert%s %s that %s did not declare in .properties.needed.",
        whichfun, ifelse(length(badprops) > 1, "ies", "y"),
        collapse(badprops, sep = ", "), name)
    } else {
      # 'adding' properties may not be present during output, but the error message
      # would be confusing if we used the 'out' message for this.
      assert(direction == "adding")
      stopf("Data returned by CPO %s has propert%s %s that %s declared in .properties.adding.\n%s",
        whichfun, ifelse(length(badprops) > 1, "ies", "y"),
        collapse(badprops, sep = ", "), name,
        paste("properties in .properties.adding may not be present in", whichfun, "output."))
    }
  }
}

##################################
### Task Splitting             ###
##################################

# Get the *indices* of columns of 'data' that are referenced by affect.* params.
# E.g. if 'affect.type == "numeric"', the indices of all numeric columns are returned.
#
# All of the parameters are just the relevantt 'affect.*' parameters as given to the CPO constructor, with the
# exception of 'data'. `getColIndices` can therefore be called using `do.call(getColIndices, insert(affect.param.list, list(data = DATA)))`
# @param data [data.frame] The data to get indices from
# @param type [character] subset of `c("numeric", "factor", "ordered", "other")`: all columns of the given type are included
# @param index [integer] index into data columns to include. Order is preserved, and they are ordered before all other matches
# @param names [character] names of data columns to include. Order is preserved, and they are ordered before other matches, except `index`
# @param pattern [character(1)] `grepl` pattern. Data columns that match the pattern are included
# @param invert [logical(1)] If TRUE, all matches are inverted, i.e. only columns that do not match any of the criteria are returned
# @param pattern.ignore.case [logical(1)] the `ignore.case` parameter of `grepl`: ignore case of `pattern`.
# @param pattern.perl [logical(1)] the `perl` parameter of `grepl`: use perl regex syntax
# @param pattern.fixed [logical(1)] the `fixed` parameter of `grepl`: don't interpret pattern as regex, but as fixed pattern.
# @return [integer]: index into `data` columns for selected columns.
getColIndices = function(data, type, index, names, pattern, invert, pattern.ignore.case, pattern.perl, pattern.fixed) {
  coltypes = vcapply(data, function(x) class(x)[1])
  coltypes[coltypes == "integer"] = "numeric"
  coltypes[!coltypes %in% c("numeric", "factor", "ordered")] = "other"
  matchcols = coltypes %in% type
  if (!is.null(pattern)) {
    matchcols = matchcols | grepl(pattern, colnames(data), pattern.ignore.case, pattern.perl, pattern.fixed)
  }
  badnames = names[!names %in% names(data)]
  if (length(badnames)) {
    stopf("Column%s not found: %s", ifelse(length(badnames) > 1, "s", ""), collapse(badnames, sep = ", "))
  }
  index = c(index, setdiff(match(names, names(data)), index))

  index = c(index, setdiff(which(matchcols), index))

  if (invert) {
    index = setdiff(seq_along(data), index)
  }
  index
}

# Translate the 'datasplit' option for internal use ("low level" datasplit) to a simplified version.
#
# most of CPOFormatCheck's split / recombine logic doesn't care about "factor", "onlyfactor", "ordered" or "numeric"
# and just treats it as "most" or "all" dataformat, subsetting the resulting data. This significantly
# simplifies the "splitting" and "recombining" of input / output data.
# E.g. if dataformat is "factor":
#  (1) the data is split according to "most" -- this is translated by 'getLLDatasplit'
#  (2) the data that is handed to the cpo.trafo function is gotten by 'getIndata' which takes the '$factor' slot of the split data, in this case
#  (3) cpo.trafo returns its output. This output is put back together with the other data using 'rebuildOutdata'
#  (4) all checks are then done as if cpo.trafo had had used the "most" dataformat and not touched any but the '$factor' slots.
# @param datasplit [character(1)] the datasplit to translate
# @return [character(1)] a simplified datasplit option
getLLDatasplit = function(datasplit) {
  if (datasplit %in% c("factor", "numeric")) {
    datasplit = "most"
  } else if (datasplit %in% c("onlyfactor", "ordered")) {
    datasplit = "all"
  }
  datasplit
}

# Get element of data according to datasplit.
#
# This is the complementary operation (category theoretically the quotient object) of `getLLDatasplit`.
# With 'indata' being split according to dataformat "factor", "onlyfactor", "ordered", or "numeric", get the relevant subitem
# from the indata after it was split according to "most" or "all".
# If dataformat is none of these, this is a noop.
# @param indata [list of data.frame | data.frame | Task] the result of splitting incoming data according to `getLLDatasplit(datasplit)`.
# @param datasplit [character(1)] one of the possible datasplit options
# @return [data.frame | Task] data formatted according to datasplit, to be fed into a trafo / retrafo function
getIndata = function(indata, datasplit) {
  if (datasplit %in% c("factor", "onlyfactor", "ordered", "numeric")) {
    indata[[ifelse(datasplit == "onlyfactor", "factor", datasplit)]]
  } else {
    indata
  }
}

# Reassemble data that was split according to some of the `datasplit` options.
#
# If dataformat is one of "factor", "onlyfactor", "ordered", or "numeric", then
# the data returned by trafo / retrafo (only the modified factors / etc) needs
# to be integrated with the remaining unmodified columns. With 'outdata' being a
# data slice according to dataformat, this function puts the returned data back
# into the "tempdata" block from which the input was taken.
#
# If dataformat is none of these, this is a noop.
# @param outdata [data.frame | list of data.frame | Task] the data returned by a trafo / retrafo function
# @param tempdata [data.frame | list of data.frame | Task] the original data, split according to `getLLDatasplit(datasplit)`.
# @param datasplit [character(1)] the datasplit option of the current CPO
# @return [data.frame | list of data.frame | Task] `outdata`, possibly embedded into `tempdata`.
rebuildOutdata = function(outdata, tempdata, datasplit) {
  if (datasplit %in% c("factor", "onlyfactor", "ordered", "numeric")) {
    tempdata[[ifelse(datasplit == "onlyfactor", "factor", datasplit)]] = outdata
    outdata = tempdata
  }
  if (datasplit %in% c("numeric", "most", "all") && is.matrix(outdata$numeric)) {
    outdata$numeric = as.data.frame(outdata$numeric)
  }
  outdata
}


# split 'outdata' into subsets given by 'which'. If 'which' does not contain "ordered", then
# 'ordered' columns are put together with 'factor' columns.
# @param which [character] subset of `c("numeric", "factor", "ordered", "other")`: by which types to split
# @param data [data.frame | any] data to split. This can also be any other list or vector if `types` is given
# @param types [character | NULL] types of columns / elements of `data`. If this is not provided, it is
#   determined from `data`. This is useful if `data` is not a data.frame but e.g. only a vector of column names.
# @return [list of data.frame | list of any] a list of subsets of `data`, named according to `which`.
splitColsByType = function(which = c("numeric", "factor", "ordered", "other"), data, types = NULL) {
  if (is.null(types)) {
    types = vcapply(data, function(x) class(x)[1])
  }
  # types: may be a character of type names, then data can be something besides a data.frame, like just a vector of names or indices
  match.arg(which, several.ok = TRUE)
  factorsubset = c("factor", if (!"ordered" %in% which) "ordered")

  sapply(which, function(x) {
    subset = if (x == "other") {
               !types %in% c("integer", "numeric", "factor", "ordered")
             } else {
               types %in% switch(x,
                 numeric = c("integer", "numeric"),
                 factor = factorsubset,
                 ordered = "ordered")
             }
    data[subset]
  }, simplify = FALSE, USE.NAMES = TRUE)
}

# This does the 'dataformat' splitting up of Tasks.
#
# Note that "dataformat" and "dataformat.factor.with.ordered" is translated to "datasplit":
# dataformat.factor.with.ordered TRUE | FALSE
# split                    --> most   | all
# factor                   --> factor | onlyfactor
#
# This performs no checks. possibly need to check that properties are adhered to
# in retrafo, must also check if the format is the same as during training
# 'possibly' here means: if not attached to a learner
#
# This is the sister of `splitdf` which gets applied to `data.frame`.
# @param task [Task] the task to split up
# @param datasplit [character(1)] subset of `c("df.features", "most", "all", "df.all", "task")`. Should be
#   a result of `getLLDatasplit` applied to the datasplit used for the CPO.
# @return [Task | data.frame | list of data.frame] the data split / formatted according to `datasplit`.
splittask = function(task, datasplit = c("df.features", "most", "all", "df.all", "task")) {
  datasplit = match.arg(datasplit)


  if (datasplit %in% c("most", "all")) {
    splt = getTaskData(task, target.extra = TRUE)

  }

  switch(datasplit,
    task = list(data = task, target = getTaskTargetNames(task)),
    df.all = list(data = getTaskData(task), target = getTaskTargetNames(task)),
    df.features = list(data = getTaskData(task, target.extra = TRUE)$data,
      target = getTaskData(task, features = character(0))),  # want the target to always be a data.frame
    most = list(data = splitColsByType(c("numeric", "factor", "other"), splt$data),
      target = getTaskData(task, features = character(0))),  # want the target to always be a data.frame
    all = list(data = splitColsByType(c("numeric", "factor", "ordered", "other"), splt$data),
      target = getTaskData(task, features = character(0))))  # want the target to always be a data.frame
}

# This does the 'dataformat' splitting up of data.frames, see `splittask`. When creating a `Task` from a
# `data.frame` for `datasplit == "task"`, a `ClusterTask` is generated.
# @param df [data.frame] the data to split up
# @param datasplit [character(1)] subset of `c("df.features", "most", "all", "df.all", "task")`. Should be
#   a result of `getLLDatasplit` applied to the datasplit used for the CPO.
# @return [Task | data.frame | list of data.frame] the data split / formatted according to `datasplit`.
splitdf = function(df, datasplit = c("df.features", "most", "all", "df.all", "task")) {
  datasplit = match.arg(datasplit)
  switch(datasplit,
    task = list(data = makeClusterTask(data = df, fixup.data = "no"), target = character(0)),
    df.all = list(data = df, target = character(0)),
    df.features = list(data = df, target = df[, character(0), drop = FALSE]),
    most = list(data = splitColsByType(c("numeric", "factor", "other"), df),
      target = df[, character(0), drop = FALSE]),
    all = list(data = splitColsByType(c("numeric", "factor", "ordered", "other"), df),
      target = df[, character(0), drop = FALSE]))
}

##################################
### Task Recombination         ###
##################################
# Task / Data recombination entails checking that data / target was only modified if allowed by the CPO type,
# checking that the number of rows didn't change, and relevant properties didn't change.

# Recombine the data previously split up by `splitdf` / `splittask` with `datasplit` being "most" or "all",
# and after the CPO trafo / retrafo function performed its operations on it.
#
# recombineLL is called by both recombinetask and recombinedf, and does the checking (e.g. number or rows did not change)
# that is common to both.
#
# 'LL' meaning 'low level'
# @param olddata [list of data.frame] data as fed to the CPO, for reference of correct row number etc.
# @param newdata [list of data.frame] data as returned by trafo / retrafo
# @param datasplit [character(1)] datasplit. This is the result of `getLLDatasplit` applied to the CPO's `datasplit`, but
#   it is a bug to call `recombineLL` if that is not "most" or "all".
# @param subset.index [integer] subset of 'task' features that were selected by 'affect.*' parameters
# @param name [character(1)] CPO name for pretty debug printing
# @return [data.frame] the data in `newdata` combined into a single data.frame.
recombineLL = function(olddata, newdata, targetnames, datasplit, subset.index, name) {
  allnames = names(olddata)
  needednames = c("numeric", "factor", "other", if (datasplit == "all") "ordered")
  if (!isTRUE(checkSetEqual(names(newdata), needednames))) {
    stopf('CPO %s gave bad return. The returned value must be a list with names {"%s"}.',
      name, collapse(needednames, sep = '", "'))
  }

  targetdata = olddata[targetnames]
  olddata = dropNamed(olddata, targetnames)
  unsubsetdata = olddata[-subset.index]
  olddata = olddata[subset.index]

  dfs = sapply(newdata, is.data.frame)
  if (any(!dfs)) {
    is.plur = sum(!dfs) > 1
    stopf("Return of %s element%s %s %s not a data.frame.", name, ifelse(is.plur, "s", ""),
      collapse(names(dfs)[!dfs], sep = ", "), ifelse(is.plur, "are", "is"))
  }

  # check no new names clash with other names
  # this kind of sucks when a CPO just happens to change the names to something thats already there
  # but we also don't want to surprise the user about us unilaterally changing names, so he needs to
  # take care of that.
  jointargetnames = c(targetnames, names(unsubsetdata), unlist(lapply(newdata, names)))
  if (any(duplicated(jointargetnames))) {
    stopf("CPO %s gave bad result\nduplicate column names %s", name, collapse(unique(jointargetnames[duplicated(jointargetnames)], sep = ", ")))
  }

  types = vcapply(olddata, function(x) class(x)[1])

  splitargetnames = splitColsByType(names(newdata), names(olddata), types) # list(numeric = [colnames], factor = [colnames]...

  numrows = nrow(olddata)
  namesorder = allnames
  for (splittype in names(splitargetnames)) {
    if (numrows != nrow(newdata[[splittype]])) {
      stopf("Number of rows of %s data returned by %s did not match input\nCPO must not change row number.",
        splittype, name)
    }
    if (!identical(splitargetnames[[splittype]], names(newdata[[splittype]]))) {
      namesorder = setdiff(namesorder, c(splitargetnames[[splittype]]))
      namesorder = c(namesorder, c(names(newdata[[splittype]])))
    }
  }

  newdata = cbind(unsubsetdata, do.call(cbind, unname(newdata)), targetdata)
  assertSetEqual(names(newdata), namesorder)
  newdata[namesorder]
}

# Recombine a task that was previously (potentially) split up according to `datasplit` and then changed by trafo / retrafo.
#
# This is used when the split up data was created from a task, and if (therefore) the result of the
# CPO is again expected to be a task.
#
# this checks that the result of trafo / retrafo has the proper type, that target and type didn't change,
# (if datasplit == "task"), and that the number of rows is the same. It then reconstructs the complete task that
# will be output by the CPO.
# @param task [Task] old task, used for input, for comparison
# @param newdata [Task | data.frame | list of data.frame] output of cpo.trafo / cpo.retrafo. This has the same format
#   as `splittask(task, datasplit)`
# @param datasplit [character(1)] the datasplit used, this is `getLLDatasplit` applied to the CPO's datasplit parameter.
# @param subset.index [integer] subset of 'task' features that were selected by 'affect.*' parameters
# @param targetbound [logical(1)] TRUE for target operating CPO, FALSE for feature operating CPO.
# @param newtasktype [character(1)] only if `targetbound`, type of new task. Give even if no task conversion happens.
# @param name [character(1)] CPO name for pretty debug printing
# @return [Task] the task incorporating the changes done by the CPO to `newdata`.
recombinetask = function(task, newdata, datasplit = c("df.all", "task", "df.features", "most", "all"),
                         subset.index, targetbound, newtasktype, name) {
  datasplit = match.arg(datasplit)

  if (is.data.frame(task)) {
    # only if 'targetbound'
    task = makeClusterTask(id = "CPO-constructed", data = task)
  }

  if (datasplit %in% c("df.features", "most", "all")) {
    if (targetbound) {
      # return is just 'target' in a df.
      if (!is.data.frame(newdata)) {
        stopf("CPO %s gave bad result\nmust return a data.frame containing the target.",
          name)
      }
      olddata = getTaskData(task)
      oldtnames = getTaskTargetNames(task)
      newtnames = names(newdata)
      if (setequal(newtnames, oldtnames)) {
        olddata[newtnames] = newdata
        newdata = olddata
      } else if (length(oldtnames) == 1 && length(newdata) == 1) {
        # note that this can NOT be combined with
        # the olddata[newtnames] block above!
        # also note the double brackets [[ ]].
        olddata[[oldtnames]] = newdata
        newdata = olddata
      } else {
        newdata = cbind(dropNamed(olddata, oldtnames), newdata)
      }
      return(constructTask(task, newdata, newtnames, newtasktype, getTaskId(task)))
    } else {
      return(changeData(task, recombinedf(getTaskData(task), newdata, datasplit, subset.index, getTaskTargetNames(task), name)))
    }
  }

  if (datasplit == "df.all") {
    checkDFBasics(task, newdata, targetbound, name)
    if (!targetbound) {
      newdata = changeData(task, newdata)
    } else {
      newdata = constructTask(task, newdata, getTaskTargetNames(task), newtasktype, getTaskId(task))
    }
  }
  checkTaskBasics(task, newdata, name)
  old.td = getTaskDesc(subsetTask(task, features = subset.index))
  new.td = getTaskDesc(newdata)

  if (targetbound) {
    checkColumnsEqual(getTaskData(task, target.extra = TRUE)$data,
      getTaskData(newdata, target.extra = TRUE)$data, "non-target column", name)
    # everything may change except size, n.feat and missings
    allowed.td.changes = setdiff(names(old.td), c("n.feat", "has.missings", "size"))
  } else {
    #check type didn't change
    assert(getTaskType(task) == getTaskType(newdata))

    # check target didn't change
    checkColumnsEqual(getTaskData(task, features = character(0)),
      getTaskData(newdata, features = character(0)), "target column", name)

    assertSetEqual(names(old.td), names(new.td))
    allowed.td.changes = c("id", "n.feat", "has.missings")
  }

  # check most of task description didn't change
  for (n in names(old.td)) {  # implicitly checks row number
    if (!n %in% allowed.td.changes && !identical(old.td[[n]], new.td[[n]])) {
      stopf("CPO %s changed task description item %s.", name, n)
    }
  }
  targetnames = getTaskTargetNames(task)
  olddata = getTaskData(task)
  fullindices = seq_along(olddata)
  aretargets = names(olddata) %in% targetnames
  new.subset.index = fullindices[!aretargets][subset.index]
  if (all(new.subset.index == sort(new.subset.index))) {
    new.subset.index = sort(c(which(aretargets), new.subset.index))
  } else {
    new.subset.index = c(which(aretargets), new.subset.index)
  }
  changeData(task, recombinedf(getTaskData(task), getTaskData(newdata), "df.all", new.subset.index, character(0), name))
}

# Recombine a data.frame that was previously (potentially) split up according to `datasplit` and then changed by trafo / retrafo.
#
# recombine data.frame after checking for match of rows etc., see 'recombinetask'.
# @param df [data.frame] old data.frame, used for input, for comparison
# @param newdata [Task | data.frame | list of data.frame] output of cpo.trafo / cpo.retrafo. This has the same format
#   as `splitdf(df, datasplit)`
# @param datasplit [character(1)] the datasplit used, this is `getLLDatasplit` applied to the CPO's datasplit parameter.
# @param subset.index [integer] subset of 'df' features that were selected by 'affect.*' parameters
# @param targetcols [character] names of target columns; this is relevant for retrafo when cpo.trafo was trained with a Task that
#   contains target columns, and cpo.retrafo is fed with a data.frame that contains columns with the same name.
# @param name [character(1)] CPO name for pretty debug printing
# @return [data.frame] the data.frame incorporating the changes done by the CPO to `newdata`
recombinedf = function(df, newdata, datasplit = c("df.features", "most", "all", "df.all", "task"), subset.index, targetcols, name) {
# otherwise it contains the columns removed from the DF because they were target columns.
  datasplit = match.arg(datasplit)
  if (datasplit %in% c("most", "all")) {
    return(recombineLL(df, newdata, targetcols, datasplit, subset.index, name))
  } else if (datasplit == "task") {
    assertClass(newdata, "Task")
    newdata = getTaskData(newdata)
  }
  if (!is.data.frame(newdata)) {
    stopf("CPO %s gave bad result\nmust return a data.frame.", name)
  }
  if (nrow(df) != nrow(newdata)) {
    stopf("CPO %s must not change number of rows.", name)
  }
  outsetcols = dropNamed(df, targetcols)[-subset.index]
  fullnames = c(names(newdata), names(outsetcols), targetcols)
  dubs = duplicated(fullnames)
  if (any(dubs)) {
    stopf("CPO %s gave bad result\ncolumn names %s duplicated (possibly with target)", name, collapse(unique(fullnames[dubs], sep = ", ")))
  }

  datanames = names(newdata)
  newdata = cbind(outsetcols, newdata, df[targetcols])
  if (identical(datanames, setdiff(names(df), targetcols)[subset.index])) {
    # names didn't change, so we preserve column order
    newdata = newdata[names(df)]
  }

  row.names(newdata) = attr(df, "row.names")
  newdata
}

# Check that columns in `old.relevants` and `new.relevants` are identical.
#
# This is mostly a helper function for pretty error messages. Depending on what
# a CPO operates on, it must not change target OR data columns. These are the "relevant"
# columns. If this rule is violated, an error message tells the user that a CPO must not
# change target / data columns.
# @param old.relevants [data.frame] subset of the old data that must stay constant
# @param new.relevants [data.frame] subset of modified data, which is checked for equality with old.relevants
# @param relevant.name [character(1)] should be something like 'targets' or 'non-target features'
# @param name [character(1)] name of the CPO for debug purposes
# @return [invisible(NULL)]
checkColumnsEqual = function(old.relevants, new.relevants, relevant.name, name) {
  if (!isTRUE(checkSetEqual(names(old.relevants), names(new.relevants)))) {
    stopf("CPO %s must not change %s names.", name, relevant.name)
  }
  for (n in names(old.relevants)) {
    if (!identical(old.relevants[[n]], new.relevants[[n]])) {
      stopf("CPO %s must not change %ss, but changed %s.", name, relevant.name, n)
    }
  }
}

# general function that builds a task of type 'type' and with id 'id', using
# the given data.
#
# 'oldtask' is only given for "classif" tasks to retain orientation of positive / negative target level,
# otherwise the data in `data` is used.
# @param oldtask [Task] the old task, used only to get task meta-information
# @param data [data.frame] the data to be used in the new task
# @param target [character] name of target columns inside `data`
# @param type [character(1)] type of the task to be created
# @param id [character(1)] id of the newly created task
# @return [Task] a new task of type `type`, with id `id`, data `data`, and other meta information from `oldtask`.
constructTask = function(oldtask, data, target, type, id) {
  if (type == "cluster") {
    return(makeClusterTask(id = id, data = data))
  }
  if (type == "classif" && getTaskType(oldtask) == "classif") {
    assert(length(target) == 1)
    oldtargets = levels(getTaskData(oldtask, target.extra = TRUE)$target)
    newtarget = levels(data[[target]])
    if (setequal(oldtargets, newtarget)) {
      positive = getTaskDesc(oldtask)$positive
      if (length(oldtargets) == 2 && oldtargets[1] != newtarget[1]) {
        positive = setdiff(oldtargets, positive)
      }
      return(makeClassifTask(id = id, data = data, target = target,
        positive = positive))
    }
  }

  constructor = switch(type,
    classif = makeClassifTask,
    multilabel = makeMultilabelTask,
    regr = makeRegrTask,
    surv = makeSurvTask)
  constructor(id = id, data = data, target = target)
}

# check that newdata is a task, and that its size fits "task"'s size.
# @param task [Task] the task to compare newdata to
# @param newdata [Task] the task to check
# @param name [character(1)] name of the CPO to use in the error message
# @return [invisible(NULL)]
checkTaskBasics = function(task, newdata, name) {
  if (!"Task" %in% class(newdata)) {
    stopf("CPO %s must return a Task", name)
  }

  if (getTaskDesc(task)$size != getTaskDesc(newdata)$size) {
    stopf("CPO %s must not change number of rows", name)
  }
}

# check that newdata is a data.frame that fits 'task's format (size, no overlap in target column names)
# @param task [Task] the task to compare newdata to
# @param newdata [data.frame] the data.frame to check
# @param targetbound [logical(1)] whether the CPO is allowed to operate on target columns
# @param name [character(1)] name of the CPO to use in the error message
# @return [invisible(NULL)]
checkDFBasics = function(task, newdata, targetbound, name) {
  if (!is.data.frame(newdata)) {
    stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return a data.frame.", name)
  }
  assertClass(newdata, "data.frame")
  tnames = getTaskTargetNames(task)
  missingt = tnames[!tnames %in% names(newdata)]
  if (length(missingt)) {
    addendum = ""
    if (targetbound) {
      addendum = paste("\nIf you want to change names or number of target columns in targetbound CPOs",
        'you must use other .dataformat values, e.g. "df.features".', sep = "\n")
    }
   stopf("CPO %s cpo.trafo gave bad result\ndata.frame did not contain target column%s %s.%s",
     name, ifelse(length(missingt) > 1, "s", ""), missingt, addendum)
  }
}

# Checks and recombines data returned by a 'retrafoless' CPO, which is allowed to operate on both
# data and target columns.
#
# perform basic checks that a retrafoless cpo returned the kind of task / data.frame that it should;
# then convert, if necessary.
# @param olddata [Task | data.frame] the original input data
# @param newdata [Task | data.frame] the data returned by the CPO trafo function
# @param datasplit [character(1)] the result of `getLLDatasplit` applied to the CPO's datasplit parameter
# @param subset.index [integer] index into olddata columns: the columns actually selected by 'affect.*' parameters
# @param name [character(1)] the CPO name used in error messages
# @return [Task | data.frame] the recombined data from newdata
recombineRetrafolessResult = function(olddata, newdata, datasplit, subset.index, name) {
  assert(identical(subset.index, seq_along(subset.index)))
  assertSubset(datasplit, c("df.all", "task"))
  if (is.data.frame(olddata)) {
    if (datasplit == "df.all") {
      assertClass(newdata, "data.frame")
    } else {  # datasplit == "task"
      assertClass(newdata, "ClusterTask")
      newdata = getTaskData(newdata)
    }
  } else {
    if (datasplit == "df.all") {
      assertClass(newdata, "data.frame")
      if (!all(getTaskTargetNames(olddata) %in% names(newdata))) {
        stopf("retrafoless CPO %s must not change target names.", name)
      }
      newdata = changeData(olddata, newdata)
    } else {  # datasplit == "task"
      assert(identical(class(newdata), class(olddata)))
      if (!all(getTaskTargetNames(olddata) == getTaskTargetNames(newdata))) {
        stopf("retrafoless CPO %s must not change target names.", name)
      }
    }
  }
  newdata
}
