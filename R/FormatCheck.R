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
# @param dataformat [character(1)] one of 'task', 'df.all', 'df.features', 'split', 'factor', 'numeric', 'ordered'
# @param strict.factors [logical(1)] whether to consider 'ordered' as separate from 'factor' types
# @param allowed.properties [character] allowed properties of `indata`
# @param subset.selector [list] information about 'affect.*' parameters that determine which subset of 'indata' is affected
# @param capture.factors [logical(1)] whether to save factor levels of input data in shapeinfo data structure. This is only used if the CPO has 'fix.factors' set to TRUE
# @param operating.type [character(1)] one of 'target', 'feature', 'retrafoless': whether target data, feature data, or both (but only during trafo) may be changed
# @param name [character(1)] name of the cpo, for message printing
# @return [list] the data to feed to the CPO trafo function, as well as meta-information:
#   list(indata = list(data, target, data.reduced, target.reduced), shapeinfo, properties, private)
#   'private' is a list containing some fields used by `handleTrafoOutput`.
prepareTrafoInput = function(indata, dataformat, strict.factors, allowed.properties, subset.selector, capture.factors, operating.type, name) {
  assert(checkClass(indata, "data.frame"), checkClass(indata, "Task"))

  subset.info = subsetIndata(indata, subset.selector, allowed.properties, "trafo", name)
  indata = subset.info$indata

  shapeinfo = makeInputShapeInfo(indata, capture.factors)
  subset.selector$data = NULL
  shapeinfo$subset.selector = subset.selector

  split.data = splitIndata(indata, dataformat, strict.factors, TRUE)


  list(indata = split.data$indata,
    shapeinfo = shapeinfo, properties = subset.info$properties,
    private = list(tempdata = split.data$tempdata, subset.index = subset.info$subset.index,
      dataformat = dataformat, strict.factors = strict.factors,
      origdata = subset.info$origdata, name = name, operating.type = operating.type))
}

# do the preparation before calling retrafo:
#  - check data is in an acceptable format (task or df)
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
#  - split the data
#  --> return
# how does mlr predict handle this stuff? they just drop target columns by name
# @param indata [Task | data.frame] incoming data to be fed to the CPO retrafo function
# @param dataformat [character(1)] one of 'task', 'df.all', 'df.features', 'split', 'factor', 'numeric', 'ordered'
# @param strict.factors [logical(1)] whether to consider 'ordered' as separate from 'factor' types
# @param allowed.properties [character] allowed properties of `indata`
# @param shapeinfo.input [InputShapeInfo] information about the data shape used to train the CPO
# @param operating.type [character(1)] one of 'target', 'feature', 'retrafoless': whether target data, feature data, or both (but only during trafo) may be changed
# @param name [character(1)] name of the cpo, for message printing
# @return [list] the data to feed to the CPO retrafo function, as well as meta-information:
#   list(indata = data in a shape fit to be fed into retrafo, properties, task, private)
#   'task' is the reconstructed task, if any
#   'private' is a list containing some fields used by `handleTrafoOutput`.
prepareRetrafoInput = function(indata, dataformat, strict.factors, allowed.properties, shapeinfo.input, operating.type, name) {
  origdata = indata
  targetop = operating.type == "target"
  # check that input column names and general types match (num / fac, or num/fac/ordered if strict.factors

  if ("factor.levels" %in% names(shapeinfo.input)) {
    indata = fixFactors(indata, shapeinfo.input$factor.levels)
  }
  task = NULL
  if ("Task" %in% class(indata)) {
    origdatatype = "Task"
    if (length(shapeinfo.input$target) && length(getTaskTargetNames(indata))) {
      # NOTE: this might be too strict: maybe the user wants to retrafo a Task with the target having a different name?
      # HOWEVER, then either the training indata's task didnt matter (and he should have trained with a data.set?), or it
      #  DID matter, in which case it is probably important to have the same data type <==> target name
      assertSetEqual(getTaskTargetNames(indata), shapeinfo.input$target, .var.name = sprintf("Target names of Task %s", getTaskId(indata)))
    }
    traintype = shapeinfo.input$type
    curtype = getTaskDesc(indata)$type
    if (traintype != curtype && traintype != "cluster" && curtype != "cluster") {
      # if either the training or the current task is of type "cluster" (could also mean
      # the training was done with a data.frame) we forgive this here.
      stopf("CPO trained with task of type %s cannot operate on task of type %s.",
        traintype, curtype)
    }
    if (curtype != "cluster" || traintype == "cluster") {
      # if the current task is a cluster, we only do target
      # op stuff if the training type was also a cluster.
      task = indata
    }
    target = getTaskData(indata, features = character(0))
    indata = getTaskData(indata, target.extra = TRUE)$data
  } else if (is.data.frame(indata)) {
    origdatatype = "data.frame"
    if (any(shapeinfo.input$target %in% names(indata)) || shapeinfo.input$type == "cluster") {
      if (!all(shapeinfo.input$target %in% names(indata))) {
        badcols = intersect(shapeinfo.input$target, names(indata))
        stopf("Some, but not all target columns of training data found in new data. This is probably an error.\n%s%s: %s",
          "Offending column", ifelse(length(badcols) > 1, "s", ""), collapse(badcols, sep = ", "))
      }
      if (targetop) {
        task = constructTask(indata, shapeinfo.input$target, shapeinfo.input$type, "[CPO CONSTRUCTED]")
      }
      target = indata[shapeinfo.input$target]
      indata = dropNamed(indata, shapeinfo.input$target)
    } else {
      target = indata[character(0)]
      shapeinfo.input$target = NULL
    }
  } else {
    stopf("Data fed into CPO %s retrafo is not a Task or data.frame.", name)
  }

  if (!is.null(task)) {
    subset.info = subsetIndata(task, shapeinfo.input$subset.selector,
      allowed.properties, "retrafo", name)
    origdata = subset.info$origdata
    indata = subset.info$indata
    assertShapeConform(getTaskData(indata, target.extra = TRUE)$data, shapeinfo.input, strict.factors, name)
  } else {
    # every kind of input looks like 'cluster' here
    allowed.properties %c=% "cluster"

    subset.info = subsetIndata(indata, shapeinfo.input$subset.selector,
      allowed.properties, "retrafo", name)
    indata = subset.info$indata
    assertShapeConform(indata, shapeinfo.input, strict.factors, name)
  }

  reducing = dataformat %in% c("task", "df.all")
  if (targetop && !is.null(task)) {
    split.data = splitIndata(indata, dataformat, strict.factors, TRUE)
    indata = split.data$indata
    if (reducing) {
      indata = list(data = indata$data.reduced,
        target = indata$data)
    } else {
      indata = indata[c("data", "target")]
    }
  } else {
    split.data = splitIndata(indata, if (reducing) "df.features" else dataformat, strict.factors, FALSE)
    indata = split.data$indata
    indata["target"] = list(NULL)  # we want a $target slot with value NULL
  }

  list(indata = indata, properties = subset.info$properties, task = task,
    private = list(tempdata = split.data$tempdata, subset.index = subset.info$subset.index,
      origdata = origdata, dataformat = dataformat, strict.factors = strict.factors,
      name = name, operating.type = operating.type, origdatatype = origdatatype,
      targetnames = names(target) %??% character(0)))
}

# Do the check of the trafo's return value
#  - check the data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check properties are allowed
#  - get a shape info object
# @param outdata [Task | data.frame | matrix] data returned by CPO trafo function
# @param prepared.input [list] return object of `prepareTrafoInput`
# @param properties.needed [character] which properties are 'needed' by the subsequent data handler.
#   Therefore, these properties may be present in `outdata` even though there were not in the input data.
# @param properties.adding [character] which properties are supposed to be 'added'
#   to the subsequent data handler. Therefore, these properties must be *absent* from `outdata`.
# @param convertto [character(1)] only if the operating.type is 'target': type of the new task
# @param simpleresult [character(1)] whethre even for df.all / task dataformat the return value will be formatted
#   according to df.features
# @return [list] the data resulting from the CPO operation, as well as meta-information: list(outdata, shapeinfo)
handleTrafoOutput = function(outdata, prepared.input, properties.needed, properties.adding, convertto, simpleresult) {
  ppr = prepared.input$private
  olddata = ppr$origdata  # incoming data that was already given to prepareTrafoInput as 'indata'
  dataformat = ppr$dataformat
  strict.factors = ppr$strict.factors
  operating.type = ppr$operating.type
  name = ppr$name
  subset.index = ppr$subset.index  # index into olddata columns: the columns actually selected by 'affect.*' parameters

  if (dataformat == "task" && !simpleresult) {
    assertTask(outdata, "trafo", name)
  }

  if (operating.type != "target") {
    # for dataformat 'factor', 'numeric' etc, combine the return object of the function with
    # all the columns that were not originally given to the function
    outdata = rebuildOutdata(outdata, ppr$tempdata, dataformat)
  }
  dataformat = getLLDataformat(dataformat)

  if (dataformat %in% c("df.all", "task") && simpleresult) {
    assert(operating.type == "feature")
    dataformat = "df.features"
  }

  recombined = if (operating.type == "target") {
    recombinetask(olddata, outdata, dataformat, strict.factors, subset.index, TRUE, convertto, name)
  } else if (operating.type == "retrafoless") {
    recombineRetrafolessResult(olddata, outdata, prepared.input$shapeinfo, dataformat, strict.factors, subset.index, name)
  } else  if (is.data.frame(olddata)) {
    # implied operating.type == "feature"
    recombinedf(olddata, outdata, dataformat, strict.factors, subset.index, character(0), name)
  } else {
    # implied operating.type == "feature"
    recombinetask(olddata, outdata, dataformat, strict.factors, subset.index, FALSE, name = name)
  }

  checkOutputProperties(outdata, recombined, prepared.input$shapeinfo$target, prepared.input$properties, properties.needed, properties.adding, operating.type, "trafo", name)

  shapeinfo = makeOutputShapeInfo(outdata)

  if ("Task" %in% class(recombined)) {
    shapeinfo$target = getTaskTargetNames(recombined)
  }

  list(outdata = recombined, shapeinfo = shapeinfo)
}

# do the check of the retrafo's return value
#  - check data is in an acceptable format (task, df, split dfs)
#  - recombine into a task / df
#  - check the properties are fulfilled
#  - check the shape is the same as during trafo
# @param outdata [Task | data.frame | matrix] data returned by CPO retrafo function
# @param prepared.input [list] return object of `prepareTrafoInput`
# @param properties.needed [character] which properties are 'needed' by the subsequent data handler.
#   Therefore, these properties may be present in `outdata` even though there were not in the input data.
# @param properties.adding [character] which properties are supposed to be 'added'
#   to the subsequent data handler. Therefore, these properties must be *absent* from `outdata`.
# @param convertto [character(1)] type of task to convert to, for target operation cpo
# @param shapeinfo.output [OutputShapeInfo] ShapeInfo describing the shape of the data returned by the CPO `trafo` function when it was called.
#        This imposes the same structure on the retrafo return value.
# @return [list] the data resulting from the CPO retrafo operation
handleRetrafoOutput = function(outdata, prepared.input, properties.needed, properties.adding, convertto, shapeinfo.output) {
  ppr = prepared.input$private
  olddata = ppr$origdata  # incoming data that was already given to prepareRetrafoInput as 'indata'
  dataformat = ppr$dataformat
  strict.factors = ppr$strict.factors
  subset.index = ppr$subset.index  # index into olddata columns: the columns actually selected by 'affect.*' parameters
  operating.type = ppr$operating.type
  targetnames = ppr$targetnames
  name = ppr$name

  if (operating.type == "target" && dataformat == "task") {
    assertTask(outdata, "retrafo", name)
  }

  # whether to ignore target columns of shapeinfo.output
  # (needed when dataformat differs between trafo & retrafo)
  drop.shapeinfo.target = dataformat %in% c("df.all", "task") && operating.type != "target"

  if (operating.type != "target") {
    # tempdata: incoming data that was given to prepareRetrafoInput as 'indata', after subsetting according to 'affect.*' parameters
    outdata = rebuildOutdata(outdata, ppr$tempdata, dataformat)
  }
  dataformat = getLLDataformat(dataformat)

  if (operating.type == "target") {
    # this won't get called at all if operating.type is target and there were not target
    # columns to rebuild. Therefore `olddata` will always be a Task here.
    recombined = recombinetask(olddata, outdata, dataformat, strict.factors, subset.index, TRUE, convertto, name)
  } else {
    if (dataformat %in% c("df.all", "task")) {
      # target is always split off during retrafo
      dataformat = "df.features"
    }
    if (ppr$origdatatype == "data.frame") {
      if (any(targetnames %in% names(olddata))) {
        assert(all(targetnames %in% names(olddata)))  # we also check this in prepareRetrafoInput
      }
      recombined = recombinedf(olddata, outdata, dataformat, strict.factors, subset.index, targetnames, name)
    } else {
      recombined = recombinetask(olddata, outdata, dataformat, strict.factors, subset.index, FALSE, name = name)
    }
  }

  checkOutputProperties(outdata, recombined, targetnames, prepared.input$properties, properties.needed, properties.adding, operating.type, "retrafo", name)

  # check the shape of outdata is as expected
  if (dataformat == "split" && operating.type != "target") {
    assertSetEqual(names(outdata), setdiff(names(shapeinfo.output), "target"))
    for (n in names(outdata)) {
      assertShapeConform(outdata[[n]], shapeinfo.output[[n]], strict.factors, name)
    }
  } else {
    if (operating.type == "target" && dataformat == "task") {
      outdata = getTaskData(outdata, target.extra = TRUE)$data
    }
    assertShapeConform(outdata, shapeinfo.output, strict.factors, name, ignore.target = drop.shapeinfo.target)
  }
  assertTargetShapeConform(recombined, shapeinfo.output, operating.type, name)

  if (operating.type == "target" && ppr$origdatatype == "data.frame") {
    # input was a data.frame (with target columns), so we return da data.frame (with target columns)
    recombined = getTaskData(recombined)
  }
  recombined
}


##################################
### Shape & Properties         ###
##################################

# make sure that the factor levels of data.frame 'data' are as described by 'levels'.
# @param data [data.frame | Task] data / task to check / modify
# @param levels [list of character] levels of `data` columns, indexed by `data` column names
# @return [data.frame | Task] the modified `data`
fixFactors = function(data, levels) {
  UseMethod("fixFactors")
}

fixFactors.default = function(data, levels) {
  assertSubset(names(levels), names(data))
  data[names(levels)] = mapply(factor, data[names(levels)], levels, SIMPLIFY = FALSE)
  data
}

fixFactors.Task = function(data, levels) {
  changeData(data, fixFactors(getTaskData(data), levels))
}

# calculate the properties of the data (only feature types & missings)
# data can be a task or data.frame
# @param data [data.frame | Task] the data to check
# @param targetnames [character] only if `data` is a data.frame: the target columns, which will be ignored
# @return [character] a subset of c("numerics", "factors", "ordered", "missings")
getDataProperties = function(data, targetnames) {
  if (is.data.frame(data)) {
    td = makeTaskDescInternal(NULL, NULL, data, targetnames, NULL, NULL, FALSE)
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
  props = getDataProperties(data, character(0))
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

# calculate properties of a general data object.
#
# This may be a Task, data.frame, or list of data.frames
# (as used with dataformat "split").
# @param data [list | data.frame | Task] The data to get properties of
# @param ignore.cols [character] names of columns to ignore, only for data.frame
# @return [character] same as of getTaskProperties
getGeneralDataProperties = function(data, ignore.cols = character(0)) {
  if ("Task" %nin% class(data) && !is.data.frame(data)) {
    unique(unlist(lapply(data, getTaskProperties)))
  } else if (is.data.frame(data)) {
    getTaskProperties(dropNamed(data, ignore.cols))
  } else {
    getTaskProperties(data)
  }
}

# give error when shape is different than dictated by shapeinfo.
#
# @param df [data.frame] the data to check
# @param shapeinfo [ShapeInfo] a the shape which `df` must conform to
# @param strict.factors [logical(1)] whether to check for 'ordered' as a type differing from 'factor'
# @param name [character(1)] name of the CPO currently being run, for error and debug printing
# @param retrafoless [logical(1)] whether this is the trafo result of a retrafoless CPO. Default FALSE.
# @param ignore.target [logical(1)] whether to ignore columns that have the same name as the target
#   column(s) declared in the $target slot. Default FALSE.
# @return [invisible(NULL)]
assertShapeConform = function(df, shapeinfo, strict.factors, name, retrafoless = FALSE,
                              ignore.target = FALSE) {
  if (ignore.target && !is.null(shapeinfo$target)) {
    shapeinfo$colnames = setdiff(shapeinfo$colnames, shapeinfo$target)
    shapeinfo$coltypes = dropNamed(shapeinfo$coltypes, shapeinfo$target)
  }
  if (!identical(names2(df), shapeinfo$colnames)) {
    errmsg = if (retrafoless) {
      "Error in CPO %s: columns may not be changed by cpo.trafo.\nInput was %s, output is %s."
    } else {
      "Error in CPO %s: column name mismatch between training and test data.\nWas %s during training, is %s now."
    }
    stopf(errmsg, name, collapse(shapeinfo$colnames, sep = ", "), collapse(names(df), sep = ", "))
  }
  indata = df[shapeinfo$colnames]

  if (strict.factors) {
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

# give error when shape recorded 'target' differs from task target
#
# only target column names are compared. This is needed for target
# operation CPOs changing target names. If data is not a Task
# or the operating.type is 'target', this does nothing.
#
# @param data [Task | data.frame] the Task.
# @param shapeinfo [ShapeInfo] a ShapeInfo
# @param operating.type [character(1)] operating type: "target", "feature", or "retrafoless"
# @return [invisible(NULL)]
assertTargetShapeConform = function(data, shapeinfo, operating.type, name) {
  if ("Task" %nin% class(data) || operating.type != "target") {
    return(invisible(NULL))
  }
  if (!identical({newtarget = getTaskTargetNames(data)}, shapeinfo$target)) {
    stopf("Error in CPO %s: Target name(s) after retrafo differ(s) from target name(s) after trafo. Was '%s', is now '%s'",
      name, collapse(newtarget, "', '"), collapse(shapeinfo$target, "', '"))
  }
}

# prepare some information about the data shape, so retrafo can check that
# it gets the kind of data it expects
# this needs to be checked both for input and for output
# @param data [data.frame] the data for which the shape is to be created
# @return [ShapeInfo] a simple datastructure that contains information about data column names and types
makeShapeInfo = function(data) {
  makeS3Obj("ShapeInfo",
    colnames = colnames(data) %??% character(0),
    coltypes = vcapply(data, function(x) class(x)[1]))
}

# like makeShapeInfo, but additionally get the target names and possibly factor levels
# @param indata [data.frame | Task] data for which the shape is to be created
# @param capture.factors [logical(1)] whether to capture factor levels
# @return [InputShapeInfo] a datastructure extending `ShapeInfo` containing information about the data shape
makeInputShapeInfo = function(indata, capture.factors) {
  if ("Task" %in% class(indata)) {
    data = getTaskData(indata, target.extra = TRUE)$data
    ret = makeShapeInfo(data)
    ret$target = getTaskTargetNames(indata)
    ret$type = getTaskDesc(indata)$type
    if (ret$type == "classif") {
      ret$positive = getTaskDesc(indata)$positive
    }
  } else {
    data = indata
    ret = makeShapeInfo(data)
    ret$target = character(0)
    ret$type = "cluster"
  }
  if (capture.factors) {
    ret$factor.levels = Filter(function(x) !is.null(x), lapply(data, levels))
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
    res$target = getTaskTargetNames(outdata)
    res$type = getTaskDesc(outdata)$type
  } else {
    # data is split by type, so we get the shape of each of the constituents
    res = lapply(outdata, makeShapeInfo)
  }
  addClasses(res, "OutputShapeInfo")
}

# check properties of data returned by trafo or retrafo function
# @param outdata [data.frame | Task | list] data returned by (re)trafo function (after rebuildOutdata)
# @param recombined [data.frame | Task] recombined data as will be returned to the user
# @param target.names [character] names of target columns
# @param input.properties [character] input properties as determined by prepare***Input
# @param properties.needed [character] which properties are 'needed' by the subsequent data handler.
#   Therefore, these properties may be present in `outdata` even though there were not in the input data.
# @param properties.adding [character] which properties are supposed to be 'added'
#   to the subsequent data handler. Therefore, these properties must be *absent* from `outdata`.
# @param operating.type [character(1)] operating type of cpo, one of 'target', 'feature', 'retrafoless'
# @param whichfun [character(1)] name of the CPO stage
# @param name [character(1)] name of the CPO
# @return [invisible(NULL)]
checkOutputProperties = function(outdata, recombined, target.names, input.properties, properties.needed, properties.adding, operating.type, whichfun, name) {
  # allowed.properties: allowed properties of `outdata`. That is the union of the CPO's 'properties.needed' field and the properties already present in 'indata'
  allowed.properties = union(input.properties, properties.needed)
  present.properties = if (operating.type == "feature") {
    getGeneralDataProperties(outdata, target.names)
  } else {
    getTaskProperties(recombined)
  }
  if (operating.type == "target") {
    # target operating CPOs can not change feature properties, but
    # there may be properties hidden from 'prepared.input$properties'
    # because of affect.*-subsetting which could be present in 'present.properties'
    # so we remove all feature properties here.
    present.properties = setdiff(present.properties, cpo.dataproperties)
  } else if (operating.type == "feature") {
    # remove properties of the target that are picked up by getGeneralDataProperties but
    # are not relevant.
    present.properties = setdiff(present.properties, cpo.all.target.properties)
  }
  assertPropertiesOk(present.properties, allowed.properties, whichfun, "out", name)
  assertPropertiesOk(present.properties, setdiff(allowed.properties, properties.adding), whichfun, "adding", name)
}

# give userfriendly error message when data does have the properties it is allowed to have.
# @param present.properties [character] properties that were found in a given data object
# @param allowed.properties [character] the properties that the data object is allowed to have
# @param whichfun [character(1)] name of the CPO stage
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

# Check that the given task does not lie about itself.
#
# This is used on user-returned tasks. Check that task.desc$size equals the row number, that
# the target names occur in the task, etc.
# @param task [Task] the task.
# @param whichfun [character(1)] which function returned the task: trafo, retrafo
# @param name [character(1)] name of the cpo
# @return [invisible(NULL)]
assertTask = function(task, whichfun, name) {
  if (!is.list(task) || !is.list(task$task.desc) || "id" %nin% names(task$task.desc)) {
    stopf("Object returned by %s %s was not a task.", whichfun, name)
  }
  taskdesignator = function() sprintf("Task %s returned by %s %s", task$task.desc$id, whichfun, name)

  if (!is.environment(task$env)) {
    stopf("%s had no environment in its '$env' slot.", taskdesignator())
  }

  task.desc = task$task.desc
  target = task.desc$target

  required.classes = switch(task.desc$type,
    classif = c("ClassifTask", "SupervisedTask"),
    regr = c("RegrTask", "SupervisedTask"),
    cluster = c("ClusterTask", "UnsupervisedTask"),
    surv = c("SurvTask", "SupervisedTask"),
    multilabel = c("MultilabelTask", "SupervisedTask"),
    stopf("%s task type must be one of classif, regr, cluster, multilabel, surv", taskdesignator()))

  if (!identical(task$type, task.desc$type)) {
    stopf("%s task type and task.desc type must be the same", taskdesignator())
  }
  required.classes = c(required.classes, "Task")
  if (!identical(required.classes, class(task))) {
    stopf("%s must have classes %s", taskdesignator(), collapse(required.classes, ", "))
  }
  required.classes = paste0(required.classes, "Desc")
  if (!identical(required.classes, class(task.desc))) {
    stopf("%s task.desc must have classes %s", taskdesignator(), collapse(required.classes, ", "))
  }

  checks = c(
      `id must be a character(1)` = testString(task.desc$id),
      `data must be a data.frame with unique column names` = testDataFrame(task$env$data, col.names = "unique"),
      `target must be a character` = testCharacter(target),
      `task.desc must have numeric 'n.feat' slot` = testNumeric(task.desc$n.feat))

  if (!all(checks)) {
    stopf("%s had problems: %s", taskdesignator(), collapse(names(checks)[!checks], "; "))
  }

  identIntLikeNum = function(x, y) identical(as.numeric(x), as.numeric(y))

  cl = table(dropNamed(vcapply(task$env$data, function(x) class(x)[1]), target))
  checks = c(
      `target must be a subset of task columns` = testSubset(target, colnames(task$env$data)),
      `number of 'numerics' features listed in task.desc is wrong` =
        identIntLikeNum(sum(cl[c("integer", "numeric")], na.rm = TRUE), task.desc$n.feat["numerics"]),
      `number of 'factors' features listed in task.desc is wrong` =
        identIntLikeNum(sum(cl["factor"], na.rm = TRUE), task.desc$n.feat["factors"]),
      `number of 'ordered' features listed in task.desc is wrong` =
        identIntLikeNum(sum(cl["ordered"], na.rm = TRUE), task.desc$n.feat["ordered"]),
      `'has.missings' slot in task.desc is wrong` =
        identical(anyMissing(task$env$data), task.desc$has.missings),
      `'size' slot in task.desc is wrong` =
        identIntLikeNum(nrow(task$env$data), task.desc$size),
      `'has.weights' slot in task.desc is wrong` =
        identical(!is.null(task$weights), task.desc$has.weights),
      `''has.blocking' slot in task.desc is wrong` =
        identical(!is.null(task$blocking), task.desc$has.blocking))
  if (!all(checks)) {
    stopf("%s had problems: %s", taskdesignator(), collapse(names(checks)[!checks], "; "))
  }

  if (task.desc$type %in% c("classif", "regr") && length(target) != 1) {
    stopf("%s is of type %s but has %s targets.", taskdesignator(), task.desc$type,
      length(target))
  } else if (task.desc$type == "surv" && length(target) != 2) {
    stopf("%s is of type surv and must have exactly two targets.", taskdesignator())
  } else if (task.desc$type == "multilabel" && length(target) < 2) {
    stopf("%s is of type multilabel and must have more than one target.", taskdesignator())
  }

  checks = switch(task.desc$type,
    classif = c(
        `class levels in task.desc are not the factor levels of the target column` =
          testSetEqual(levels(task$env$data[[target]]), task.desc$class.levels),
        `task.desc 'positive' and 'negative' slots must be NA for multiclass tasks` =
          length(task.desc$class.levels) <= 2 || (is.na(task.desc$positive) && is.na(task.desc$negative)),
        `task.desc 'positive' and 'negative' slots must be both class levels of the target` =
          length(task.desc$class.levels) != 2 || (
            testString(task.desc$positive) &&
            testString(task.desc$negative) &&
            testSetEqual(c(task.desc$positive, task.desc$negative), task.desc$class.levels)),
        `task.desc 'positive' slot must be the class level, 'negative' slot must be not_<positive>` =
          length(task.desc$class.levels) != 1 || (
            testString(task.desc$positive) &&
            testString(task.desc$negative) &&
            identical(task.desc$positive, task.desc$class.levels) &&
            identical(task.desc$negative, paste0("not_", task.desc$class.levels)))),
    regr = TRUE,
    cluster = TRUE,
    surv = c(
        `time column must be numeric` = testNumeric(task$env$data[[target[1]]]),
        `event column must be logical` = testLogical(task$env$data[[target[2]]])),
    multilabel = c(
        `class.levels in task.desc must equal target names.` =
          testSetEqual(task.desc$class.levels, target)),
    stop("Unexpected error: task.desc$type was bad."))

  if (!all(checks)) {
    stopf("%s had problems: %s", taskdesignator(), collapse(names(checks)[!checks], "; "))
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
  index %c=% setdiff(match(names, names(data)), index)

  index %c=% setdiff(which(matchcols), index)

  if (invert) {
    index = setdiff(seq_along(data), index)
  }
  index
}

# Translate the 'dataformat' option for internal use ("low level" dataformat) to a simplified version.
#
# most of CPOFormatCheck's split / recombine logic doesn't care about "factor", "onlyfactor", "ordered" or "numeric"
# and just treats it as "most" or "all" dataformat, subsetting the resulting data. This significantly
# simplifies the "splitting" and "recombining" of input / output data.
# E.g. if dataformat is "factor":
#  (1) the data is split according to "most" -- this is translated by 'getLLDataformat'
#  (2) the data that is handed to the cpo.trafo function is gotten by 'getIndata' which takes the '$factor' slot of the split data, in this case
#  (3) cpo.trafo returns its output. This output is put back together with the other data using 'rebuildOutdata'
#  (4) all checks are then done as if cpo.trafo had had used the "most" dataformat and not touched any but the '$factor' slots.
# @param dataformat [character(1)] the dataformat to translate
# @return [character(1)] a simplified dataformat option
getLLDataformat = function(dataformat) {
  if (dataformat %in% c("factor", "numeric", "ordered")) {
    "split"
  } else {
    dataformat
  }
}

# Get element of data according to dataformat.
#
# This is the complementary operation (category theoretically the quotient object) of `getLLDataformat`.
# With 'indata' being split according to dataformat "factor", "onlyfactor", "ordered", or "numeric", get the relevant subitem
# from the indata after it was split according to "most" or "all".
# If dataformat is none of these, this is a noop.
# @param indata [list of data.frame | data.frame | Task] the result of splitting incoming data according to `getLLDataformat(dataformat)`.
# @param dataformat [character(1)] one of the possible dataformat options
# @return [data.frame | Task] data formatted according to dataformat, to be fed into a trafo / retrafo function
getIndata = function(indata, dataformat) {
  if (dataformat %in% c("factor", "ordered", "numeric")) {
    indata[[dataformat]]
  } else {
    indata
  }
}

# Reassemble data that was split according to some of the `dataformat` options.
#
# If dataformat is one of "factor", "onlyfactor", "ordered", or "numeric", then
# the data returned by trafo / retrafo (only the modified factors / etc) needs
# to be integrated with the remaining unmodified columns. With 'outdata' being a
# data slice according to dataformat, this function puts the returned data back
# into the "tempdata" block from which the input was taken.
#
# If dataformat is none of these, this is a noop.
# @param outdata [data.frame | list of data.frame | Task] the data returned by a trafo / retrafo function
# @param tempdata [data.frame | list of data.frame | Task] the original data, split according to `getLLDataformat(dataformat)`.
# @param dataformat [character(1)] the dataformat option of the current CPO
# @return [data.frame | list of data.frame | Task] `outdata`, possibly embedded into `tempdata`.
rebuildOutdata = function(outdata, tempdata, dataformat) {
  if (dataformat %in% c("factor", "ordered", "numeric")) {
    tempdata[[dataformat]] = outdata
    outdata = tempdata
  }
  if (dataformat %in% c("numeric", "split") && is.matrix(outdata$numeric)) {
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


# Convenience function for 'dataformat' splitting.
#
# calls `splitdf` or `splittask`, depending on datatype of `data`.
#
# This performs no checks. possibly need to check that properties are adhered to
# in retrafo, must also check if the format is the same as during training
# 'possibly' here means: if not attached to a learner
#
# @param data [Task | data.frame] the data to split up
# @param dataformat [character(1)] subset of `c("df.features", "split", "df.all", "task")`. Should be
#   a result of `getLLDataformat` applied to the dataformat used for the CPO.
# @param strict.factors [logical(1)] whether to split ordered from factor columns
# @return [Task | data.frame | list of data.frame] the data split / formatted according to `dataformat`.
splitX = function(data, dataformat = c("df.features", "split", "df.all", "task"), strict.factors) {
  dataformat = match.arg(dataformat)
  if (is.data.frame(data)) {
    splitdf(data, dataformat, strict.factors)
  } else {
    splittask(data, dataformat, strict.factors)
  }
}

# check whether the first level of a classif target is not the positive level
#
# @param task [Task] the task to check
# @return [logical(1)] TRUE when the first level of a classif target is not the positive level, FALSE otherwise, and
#   for non-classif task.
isLevelFlipped = function(task) {
  if (getTaskType(task) != "classif") {
    return(FALSE)
  }
  pos = getTaskDesc(task)$positive
  assert(!is.null(pos))
  if (is.na(pos)) {
    return(FALSE)
  }
  target = getTaskData(task, target.extra = TRUE)$target
  assert(length(levels(target)) <= 2)
  if (!identical(levels(target)[1], pos)) {
    assert(identical(levels(target)[2], pos))
    return(TRUE)
  }
  FALSE
}

# reorder the levels of a classif target to make the positive level the first one
#
# @param data [data.frame] the data frame containing the target
# @param target [character(1) | numeric(1)] the name or index of the target column. Is assumed to be a factor with two levels.
# @return [data.frame] the input data with the two levels of the target column flipped.
flipTaskTarget = function(data, target) {
  data[[target]] = factor(data[[target]], levels = rev(levels(data[[target]])))
  data
}

# reorder levels of classif target if the original task was level flipped
#
# @param data [data.frame] the data frame containing the target to be flipped
# @param task [Task] the original task. The target column of this task must be the one of the data.frame
# @return [data.frame] the input data, potentially with the two levels of the target column flipped.
unflipTarget = function(data, task) {
  if (isLevelFlipped(task)) {
    data = flipTaskTarget(data, getTaskTargetNames(task))
  }
  data
}


# This does the 'dataformat' splitting up of Tasks.
#
# This is the sister of `splitdf` which gets applied to `data.frame`.
# @param task [Task] the task to split up
# @param dataformat [character(1)] subset of `c("df.features", "split", "df.all", "task")`. Should be
#   a result of `getLLDataformat` applied to the dataformat used for the CPO.
# @param strict.factors [logical(1)] whether to split ordered from factor columns
# @return [Task | data.frame | list of data.frame] the data split / formatted according to `dataformat`.
splittask = function(task, dataformat, strict.factors) {
  if (dataformat %in% c("split", "df.features")) {
    splt = getTaskData(task, target.extra = TRUE)$data
    colsplit = c("numeric", "factor", if (strict.factors) "ordered", "other")
    trg = getTaskData(task, features = character(0))
    if (isLevelFlipped(task)) {
      trg = flipTaskTarget(trg, 1)
    }
  }
  if (dataformat == "df.all") {
    data = getTaskData(task)
    target = getTaskTargetNames(task)
    data = unflipTarget(data, task)
    return(list(data = data, target = target))
  }

  switch(dataformat,
    task = list(data = task, target = getTaskTargetNames(task)),
    df.features = list(data = splt,
      target = trg),  # want the target to always be a data.frame
    split = list(data = splitColsByType(colsplit, splt),
      target = trg))  # want the target to always be a data.frame
}

# This does the 'dataformat' splitting up of data.frames.
#
# When creating a `Task` from a `data.frame` for `dataformat == "task"`, a `ClusterTask` is generated.
# @param df [data.frame] the data to split up
# @param dataformat [character(1)] subset of `c("df.features", "split", "df.all", "task")`. Should be
#   a result of `getLLDataformat` applied to the dataformat used for the CPO.
# @param strict.factors [logical(1)] whether to split ordered from factor columns
# @return [Task | data.frame | list of data.frame] the data split / formatted according to `dataformat`.
splitdf = function(df, dataformat, strict.factors) {
  colsplit = c("numeric", "factor", if (strict.factors) "ordered", "other")
  switch(dataformat,
    task = list(data = makeClusterTask("[CPO CONSTRUCTED]", data = df, fixup.data = "no", check.data = FALSE), target = character(0)),
    df.all = list(data = df, target = character(0)),
    df.features = list(data = df, target = df[, character(0), drop = FALSE]),
    split = list(data = splitColsByType(colsplit, df),
      target = df[, character(0), drop = FALSE]))
}

# Take subset of data according to 'affect.*' parameters
#
# @param indata [Task | data.frame]
# @param subset.selector [list] information about 'affect.*' parameters that determine which subset of 'indata' is affected
# @param allowed.properties [character] allowed properties of `indata`
# @param whichfun [character(1)] name of the CPO stage
# @param cpo.name [character(1)] name of the CPO
# @return [list] list(origdata, indata, subset.index, properties)
subsetIndata = function(indata, subset.selector, allowed.properties, whichfun, cpo.name) {
  origdata = indata
  if ("Task" %in% class(indata)) {
    subset.selector$data = getTaskData(indata, target.extra = TRUE)$data
    subset.index = do.call(getColIndices, subset.selector)
    # subsetTask, but keep everything in order
    new.subset.index = featIndexToTaskIndex(subset.index, indata)
    indata.data = getTaskData(indata)
    if (!identical(as.integer(new.subset.index), seq_along(indata.data))) {
      indata = changeData(indata, indata.data[new.subset.index])
    }
  } else {
    subset.selector$data = indata
    subset.index = do.call(getColIndices, subset.selector)
    indata = indata[subset.index]
  }

  present.properties = getTaskProperties(indata)
  assertPropertiesOk(present.properties, allowed.properties, whichfun, "in", cpo.name)

  list(origdata = origdata, indata = indata, subset.index = subset.index,
    properties = present.properties)
}


# Split cpo input data according to dataformat
#
# Creates also 'tempdata', the data after the split but before
# subsetting (useful for dataformat 'numeric', 'factors' etc)
# and possibly 'reduced.indata', which reduces df.all and task into df.features.
# @param data [data.frame | Task] the input data
# @param dataformat [character(1)] one of 'task', 'df.all', 'df.features', 'split', 'factor', 'numeric', 'ordered'
# @param strict.factors [logical(1)] whether to consider 'ordered' as separate from 'factor' types
# @param create.reduced [logical(1)] whether to create 'reduced' indata
# @return [list] list(indata, tempdata). indata is the proper input for the CPO function,
#   a list(data, target [, data.reduced, target.reduced]). tempdata the data after split before subsetting.
splitIndata = function(data, dataformat, strict.factors, create.reduced) {
  indata = splitX(data, getLLDataformat(dataformat), strict.factors)
  tempdata = indata$data
  indata$data = getIndata(indata$data, dataformat)

  if (create.reduced) {
    # create separate "reduced" data that, besides containing the full task / df, also
    # contains the data and target alone.
    reduced.indata = if (dataformat %in% c("task", "df.all")) {
      splitX(data, "df.features", strict.factors)
    } else {
      indata
    }
    names(reduced.indata) = paste0(names(reduced.indata), ".reduced")
    indata %c=% reduced.indata
  }
  list(indata = indata, tempdata = tempdata)
}


##################################
### Task Recombination         ###
##################################
# Task / Data recombination entails checking that data / target was only modified if allowed by the CPO type,
# checking that the number of rows didn't change, and relevant properties didn't change.

# Recombine the data previously split up by `splitdf` / `splittask` with `dataformat` being "most" or "all",
# and after the CPO trafo / retrafo function performed its operations on it.
#
# recombineLL is called by both recombinetask and recombinedf, and does the checking (e.g. number or rows did not change)
# that is common to both.
#
# 'LL' meaning 'low level'
# @param olddata [list of data.frame] data as fed to the CPO, for reference of correct row number etc.
# @param newdata [list of data.frame] data as returned by trafo / retrafo
# @param subset.index [integer] subset of 'task' features that were selected by 'affect.*' parameters
# @param name [character(1)] CPO name for pretty debug printing
# @return [data.frame] the data in `newdata` combined into a single data.frame.
recombineLL = function(olddata, newdata, targetnames, strict.factors, subset.index, name) {
  allnames = names(olddata)
  needednames = c("numeric", "factor", "other", if (strict.factors) "ordered")
  if (!isTRUE(checkSetEqual(names(newdata), needednames))) {
    stopf('CPO %s gave bad return. The returned value must be a list with names {"%s"}.',
      name, collapse(needednames, sep = '", "'))
  }

  targetdata = olddata[targetnames]
  olddata = dropNamed(olddata, targetnames)
  unsubsetdata = olddata[-subset.index]
  olddata = olddata[subset.index]

  dfs = vlapply(newdata, is.data.frame)
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
      namesorder = setdiff(namesorder, splitargetnames[[splittype]])
      namesorder %c=% names(newdata[[splittype]])
    }
  }

  newdata = cbind(unsubsetdata, do.call(cbind, unname(newdata)), targetdata)
  assertSetEqual(names(newdata), namesorder)
  newdata[namesorder]
}

# Recombine a task that was previously (potentially) split up according to `dataformat` and then changed by trafo / retrafo.
#
# This is used when the split up data was created from a task, and if (therefore) the result of the
# CPO is again expected to be a task.
#
# this checks that the result of trafo / retrafo has the proper type, that target and type didn't change,
# (if dataformat == "task"), and that the number of rows is the same. It then reconstructs the complete task that
# will be output by the CPO.
# @param task [Task] old task, used for input, for comparison
# @param newdata [Task | data.frame | list of data.frame] output of cpo.trafo / cpo.retrafo. This has the same format
#   as `splittask(task, dataformat)`
# @param dataformat [character(1)] the dataformat used, this is `getLLDataformat` applied to the CPO's dataformat parameter.
# @param strict.factors [logical(1)] whether to consider 'ordered' as separate from 'factor' types
# @param subset.index [integer] subset of 'task' features that were selected by 'affect.*' parameters
# @param targetbound [logical(1)] TRUE for target operating CPO, FALSE for feature operating CPO.
# @param newtasktype [character(1)] only if `targetbound`, type of new task. Give even if no task conversion happens.
# @param name [character(1)] CPO name for pretty debug printing
# @return [Task] the task incorporating the changes done by the CPO to `newdata`.
recombinetask = function(task, newdata, dataformat = c("df.all", "task", "df.features", "split"),
                         strict.factors, subset.index, targetbound, newtasktype, name) {
  dataformat = match.arg(dataformat)

  if (is.data.frame(task)) {
    # only if 'targetbound'
    task = makeClusterTask(id = "[CPO CONSTRUCTED]", data = task, fixup.data = "no", check.data = FALSE)
  }

  if (dataformat %in% c("df.features", "split")) {
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
        assert(length(oldtnames) == 1)
        # note that this can NOT be combined with
        # the olddata[newtnames] block above!
        # also note the double brackets [[ ]].
        olddata[[oldtnames]] = newdata[[1]]
        names(olddata)[names(olddata) == oldtnames] = names(newdata)
        newdata = olddata
      } else {
        newdata = cbind(dropNamed(olddata, oldtnames), newdata)
      }
      if (anyDuplicated(colnames(newdata))) {
        stopf("CPO %s introduced duplicate column names", name)
      }
      if (newtasktype == "classif") {
        newdata = unflipTarget(newdata, task)
      }
      return(constructTask(newdata, newtnames, newtasktype, getTaskId(task), isLevelFlipped(task)))
    } else {
      return(changeData(task, recombinedf(getTaskData(task), newdata, dataformat, strict.factors, subset.index, getTaskTargetNames(task), name)))
    }
  }

  if (dataformat == "df.all") {
    checkDFBasics(task, newdata, targetbound, name)
    if (!targetbound) {
      newdata = unflipTarget(newdata, task)
      newdata = changeData(task, newdata)
    } else {
      if (newtasktype == "classif") {
        newdata = unflipTarget(newdata, task)
      }
      newdata = constructTask(newdata, getTaskTargetNames(task), newtasktype, getTaskId(task), isLevelFlipped(task))
    }
  }

  if (nrow(getTaskData(task)) != nrow(getTaskData(newdata))) {
    stopf("CPO %s must not change number of rows", name)
  }

  new.subset.index = featIndexToTaskIndex(subset.index, task)
  if (targetbound) {
    # everything may change except size, n.feat and missings
    fulldata = recombinedf(getTaskData(task), getTaskData(newdata), "df.all", strict.factors, new.subset.index, character(0), name)
    fulltask = constructTask(fulldata, getTaskTargetNames(newdata), newtasktype, getTaskId(newdata), isLevelFlipped(newdata))
    checkColumnsEqual(getTaskData(task, target.extra = TRUE)$data[subset.index],
      getTaskData(newdata, target.extra = TRUE)$data, "non-target column", name)
    checkTaskBasics(task, fulltask, setdiff(names(getTaskDesc(task)), c("n.feat", "has.missings", "has.blocking", "has.weights")), name)
    return(fulltask)
  }
  #check type didn't change
  assert(getTaskType(task) == getTaskType(newdata))
  assertSetEqual(names(getTaskDesc(task)), names(getTaskDesc(newdata)))

  # check target didn't change
  checkColumnsEqual(getTaskData(task, features = character(0)),
    getTaskData(newdata, features = character(0)), "target column", name)

  checkTaskBasics(subsetTask(task, features = subset.index), newdata, c("id", "n.feat", "has.missings"), name)


  changeData(task, recombinedf(getTaskData(task), getTaskData(newdata), "df.all", strict.factors, new.subset.index, character(0), name))
}


# convert an index of feature columns to an index w.r.t. the whole task
#
# A column index that references columns with respect the data
# columns only is converted to the column index with respect the
# whole task data.frame (including target columns).
#
# Target columns are included in this index. If feat.index
# is a sorted numeric, the target columns just get sorted into
# the feat.index; otherwise they are put at the beginning.
# @param feat.index [numeric] index of columns with respect to feature cols only
# @param task [Task] the task
# @return [numeric] index w.r.t. the whole task df. Includes target cols.
featIndexToTaskIndex = function(feat.index, task) {
  task.data = getTaskData(task)
  fullindex = seq_along(task.data)
  aretargets = names(task.data) %in% getTaskTargetNames(task)
  new.subset.index = fullindex[!aretargets][feat.index]
  if (all(new.subset.index == sort(new.subset.index))) {
    sort(c(which(aretargets), new.subset.index))
  } else {
    c(which(aretargets), new.subset.index)
  }
}

# Recombine a data.frame that was previously (potentially) split up according to `dataformat` and then changed by trafo / retrafo.
#
# recombine data.frame after checking for match of rows etc., see 'recombinetask'.
# @param df [data.frame] old data.frame, used for input, for comparison
# @param newdata [Task | data.frame | list of data.frame] output of cpo.trafo / cpo.retrafo. This has the same format
#   as `splitdf(df, dataformat)`
# @param dataformat [character(1)] the dataformat used, this is `getLLDataformat` applied to the CPO's dataformat parameter.
# @param strict.factors [logical(1)] whether to consider 'ordered' as separate from 'factor' types
# @param subset.index [integer] subset of 'df' features that were selected by 'affect.*' parameters
# @param targetcols [character] names of target columns; this is relevant for retrafo when cpo.trafo was trained with a Task that
#   contains target columns, and cpo.retrafo is fed with a data.frame that contains columns with the same name.
# @param name [character(1)] CPO name for pretty debug printing
# @return [data.frame] the data.frame incorporating the changes done by the CPO to `newdata`
recombinedf = function(df, newdata, dataformat = c("df.features", "split", "df.all", "task"), strict.factors, subset.index, targetcols, name) {
# otherwise it contains the columns removed from the DF because they were target columns.
  dataformat = match.arg(dataformat)
  if (dataformat == "split") {
    return(recombineLL(df, newdata, targetcols, strict.factors, subset.index, name))
  } else if (dataformat == "task") {
    assertClass(newdata, "Task")
    newdata = getTaskData(newdata)
  }
  if (!is.data.frame(newdata)) {
    stopf("CPO %s gave bad result\nmust return a data.frame.", name)
  }
  if (nrow(df) != nrow(newdata)) {
    stopf("CPO %s must not change number of rows.", name)
  }
  outsetcols = dropNamed(df, targetcols)
  if (length(subset.index)) {
    outsetcols = outsetcols[-subset.index]
  }
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
    names(newdata) = names(df)
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
# @param data [data.frame] the data to be used in the new task
# @param target [character] name of target columns inside `data`
# @param type [character(1)] type of the task to be created
# @param id [character(1)] id of the newly created task
# @param flip [logical(1)] whether, for binary classif task, to put the 2nd level on 'positive'
# @return [Task] a new task of type `type`, with id `id`, data `data`, and other meta information from `oldtask`.
constructTask = function(data, target, type, id, flip = FALSE) {
  if (type == "cluster") {
    if (length(target)) {
      stop("Cluster task cannot have target columns")
    }
    return(makeClusterTask(id = id, data = data, fixup.data = "no", check.data = FALSE))
  }
  if (type == "classif") {
    assertString(target)
    targetcol = data[[target]]
    if (!is.factor(targetcol)) {
      stop("ClassifTask target must be a factor column!")
    }
    if (flip && length(target) == 1) {
      if (length(levels(targetcol)) == 2) {
        positive = levels(targetcol)[2]
        return(makeClassifTask(id = id, data = data, target = target,
          positive = positive, fixup.data = "no", check.data = FALSE))
      }
    }
  }
  constructor = switch(type,
    classif = makeClassifTask,
    multilabel = makeMultilabelTask,
    regr = makeRegrTask,
    surv = makeSurvTask)
  constructor(id = id, data = data, target = target, fixup.data = "no", check.data = FALSE)
}

# check that newdata is a task, and that it agrees with the
# old 'task' on everything except 'allowed.td.changes'
#
# @param task [Task] the task to compare newdata to
# @param newdata [Task] the task to check
# @param allowed.td.changes [character] slots of 'task.desc' that the tasks may disagree on
# @param name [character(1)] name of the CPO to use in the error message
# @return [invisible(NULL)]
checkTaskBasics = function(task, newdata, allowed.td.changes, name) {
  if (!"Task" %in% class(newdata)) {
    stopf("CPO %s must return a Task", name)
  }

  if ("size" %nin% allowed.td.changes && getTaskDesc(task)$size != getTaskDesc(newdata)$size) {
    stopf("CPO %s must not change number of rows", name)
  }

  old.td = getTaskDesc(task)
  new.td = getTaskDesc(newdata)

  # check most of task description didn't change
  for (n in setdiff(names(old.td), allowed.td.changes)) {
    if (!isTRUE({complaint = all.equal(old.td[[n]], new.td[[n]])})) {
      stopf("CPO %s changed task description item %s:\n%s", name, n, complaint)
    }
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
        'you must use other dataformat values, e.g. "df.features".', sep = "\n")
    }
   stopf("CPO %s cpo.trafo gave bad result\ndata.frame did not contain target column%s %s.%s",
     name, ifelse(length(missingt) > 1, "s", ""), collapse(missingt, ", "), addendum)
  }
}

# Checks and recombines data returned by a 'retrafoless' CPO, which is allowed to operate on both
# data and target columns.
#
# perform basic checks that a retrafoless cpo returned the kind of task / data.frame that it should;
# then convert, if necessary.
# @param olddata [Task | data.frame] the original input data
# @param newdata [Task | data.frame] the data returned by the CPO trafo function
# @param shapeinfo [ShapeInfo] The input shape which `df` must conform to
# @param dataformat [character(1)] the result of `getLLDataformat` applied to the CPO's dataformat parameter
# @param strict.factors [logical(1)] whether to check for 'ordered' as a type differing from 'factor'
# @param subset.index [integer] index into olddata columns: the columns actually selected by 'affect.*' parameters
# @param name [character(1)] the CPO name used in error messages
# @return [Task | data.frame] the recombined data from newdata
recombineRetrafolessResult = function(olddata, newdata, shapeinfo.input, dataformat, strict.factors, subset.index, name) {
  assert(identical(subset.index, seq_along(subset.index)))
  assertSubset(dataformat, c("df.all", "task"))
  if (is.data.frame(olddata)) {
    if (dataformat == "df.all") {
      assertClass(newdata, "data.frame")
    } else {  # dataformat == "task"
      assertClass(newdata, "ClusterTask")
      newdata = getTaskData(newdata)
    }
    assertShapeConform(newdata, shapeinfo.input, strict.factors, name, TRUE)
  } else {
    if (dataformat == "df.all") {
      assertClass(newdata, "data.frame")
      if (!all(getTaskTargetNames(olddata) %in% names(newdata)) ||
          !all(names(newdata)[names(newdata) %in% getTaskTargetNames(olddata)] == getTaskTargetNames(olddata))) {
        stopf("retrafoless CPO %s must not change target names.", name)
      }
      if (getTaskType(olddata) == "classif") {
        tname = getTaskTargetNames(olddata)
        if (isLevelFlipped(olddata)) {
          newdata = flipTaskTarget(newdata, tname)
        }
        if (!identical(levels(getTaskData(olddata, target.extra = TRUE)$target),
          levels(newdata[[tname]]))) {
          stopf("retrafoless CPO %s must not change target class levels.", name)
        }
      }
      newdata = changeData(olddata, newdata)
    } else {  # dataformat == "task"
      if (!identical(class(newdata), class(olddata))) {
        stopf("retrafoless CPO %s must not change task type.", name)
      }
      if (!all(getTaskTargetNames(olddata) == getTaskTargetNames(newdata))) {
        stopf("retrafoless CPO %s must not change target names.", name)
      }
      if (getTaskType(olddata) == "classif") {
        if (isLevelFlipped(olddata) != isLevelFlipped(newdata)) {
          stopf("CPO %s changed task target feature order.", name)
        }
        if (!identical(levels(getTaskData(olddata, target.extra = TRUE)$target),
          levels(getTaskData(newdata, target.extra = TRUE)$target))) {
          stopf("retrafoless CPO %s must not change target class levels.", name)
        }
      }
      checkTaskBasics(olddata, newdata, c("has.missings", "size", "class.distribution"), name)
    }
    assertShapeConform(getTaskData(newdata, target.extra = TRUE)$data, shapeinfo.input, strict.factors, name, TRUE)
  }
  newdata
}



