

# train, retrafo, traininvert, invert:
# should all have parameter 'param',
# retrafo, traininvert, invert should have 'control'
# train, invert should have 'target'
# train, retrafo, traininvert should have 'data'
# invert should have 'predict.type'
# may be omitted if not needed
# fr - functional retrafo
# fi - functional invert
# sl - stateless
# ci - constant invert
# sl and ci influence how often train & traininvert are called.
# dataformat is checked automatically
# properties.adding, properties.needed, properties.target are filled automatically if not given
generalMakeCPO = function(name, train, retrafo, traininvert = NULL, invert = NULL,
  type = c("simple", "extended", "target", "target.extended", "retrafoless"),
  fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE, dataformat = "df.features",
  dataformat.factor.with.ordered = TRUE,
  convertfrom = "regr", convertto = convertfrom, properties.data = c("numerics", "factors", "ordered", "missings"),
  properties.adding = NULL, properties.needed = NULL, properties.target = NULL, predict.type.map = cpo.identity.predict.type.map)  {

  istocpo = type %in% c("target", "target.extended")

  if (is.null(properties.target)) {
    if (istocpo) {
      properties.target = convertfrom
      if (convertfrom == "classif") {
        properties.target %c=% c("oneclass", "twoclass", "multiclass")
      }
    } else {
      properties.target = c(tasktypes, cpo.targetproperties)
    }
  }

  if (is.null(properties.adding)) {
    if (istocpo && convertfrom != convertto) {
      properties.adding = properties.target
    } else {
      properties.adding = character(0)
    }
  }

  if (is.null(properties.needed)) {
    if (istocpo && convertfrom != convertto) {
      properties.needed = convertto
      if (convertto == "classif") {
        properties.needed %c=% c("twoclass", "multiclass")
      }
    } else {
      properties.needed = character(0)
    }
  }

  ps = pSS(param = 1: integer[, ])

  reduceDataformat = function(data, target, whichfun) {
    # turn data into a df.all df
    if (dataformat %in% c("factor", "ordered", "numeric")) {
      dataformat = "df.features"
    }

    if ("task" %in% class(data)) {
      assert(identical(target, getTaskTargetNames(data)))
    }

    if (whichfun == "invert") {
      assertNull(data)
      if (!is.data.frame(target)) {
        target = data.frame(target.converted = target)
      }
      return(list(data = data, target = target))
    }
    if (whichfun == "traininvert") {
      assertNull(target)
      target = character(0)
      if (dataformat == "task" && class(data) %in% "Task") {
        data = getTaskData(data, target.extra = TRUE)$data
        dataformat = "df.features"
      } else if (dataformat == "df.all") {
        dataformat = "df.features"
      }
    }
    if (whichfun == "train") {
      if (!sl && type != "retrafoless") {
        assert(!is.null(target))
      } else {
        assertNull(target)
        if (dataformat %in% c("task", "df.all")) {
          dataformat = "df.features"
          target = character(0)
        }
      }
    }
    if (whichfun == "retrafo") {
      if (type %in% c("target", "target.extended")) {
        if (dataformat %in% c("df.all", "task")) {
          assert(identical(data, getTaskData(target, target.extra = TRUE)$data))
          data = target
        }
      } else {
        if (dataformat == "task") {
          if ("task" %in% class(data)) {
            assert(!is.null(target))
            data = getTaskData(data, target.extra = TRUE)$data
          }
          dataformat = "df.features"
        } else if (dataformat == "df.all") {
          if (!is.null(target)) {
            data = dropNamed(data, target)
          }
          dataformat = "df.features"
        }
        target = character(0)
      }
    }

    switch(dataformat,
      task = {
        target = getTaskData(data, features = character(0))
        data = getTaskData(data, target.extra = TRUE)$data
      },
      df.all = {
        assertSubset(target, colnames(data))
        target.names = target
        target = data[target.names]
        data = dropNamed(data, target.names)
      })
    if (dataformat == "split") {
      data = sapply(names(data), function(x) {
        ret = data[[x]]
        names(ret) = paste(x, names(ret), sep = ".")
        ret
      }, simplify = FALSE)
    } else {
      coltypes = BBmisc::vcapply(data, function(x) class(x)[1])
      if (dataformat.factor.with.ordered) {
        coltypes[coltypes == "ordered"] = "factor"
      }
      coltypes[coltypes == "integer"] = "numeric"
      coltypes[!coltypes %in% c("numeric", "factor", "ordered")] = "other"
      names(coltypes) = NULL
      data = sapply(c("numeric", "factor", if (!dataformat.factor.with.ordered) "ordered", "other"), function(x) {
        data = data[coltypes == x]
        names(data) = paste(x, names(data), sep = ".")
        data
      }, simplify = FALSE)
    }
    data = do.call(cbind, data)
    names(target) = paste("target", names(target), sep = ".")
    data = cbind(data, target)
    target = names(target)

    list(data = data, target = target)
  }

  addDataformat = function(result, olddata, oldtarget, whichfun, predict.type) {
    # turn data back into original format
    # only for 'retrafo', 'invert'

    if (dataformat %in% c("factor", "ordered", "numeric")) {
      dataformat = "df.features"
    }

    if (whichfrun == "retrafo") {
      assert(is.null(oldtarget) == !istocpo)
      if (dataformat %in% c("df.all", "task") && is.null(oldtarget)) {
        dataformat = "df.features"
      }

      if (istocpo && !dataformat %in% c("df.all", "task")) {
        result = result[grepl("^target\\.", names(result))]
        names(result) = gsub("^target\\.", names(result))
        return(result)
      }
      assert(istocpo || dataformat %in% c("df.all", "task") || !any(grepl("^target\\.", names(result))))
      if (dataformat == "split") {
        result = sapply(c("numeric", "factor", if (!dataformat.factor.with.ordered) "ordered", "other"), function(x) {
          result = result[grepl(paste0("^", x, "\\."), names(result))]
          names(result) = gsub(paste0("^", x, "\\."), "", names(result))
          result
        })
      } else if (dataformat == "df.features") {
        result = result[grepl("^target\\.", colnames(result), invert = !istocpo)]
        colnames(result) = gsub(paste0("^(numeric|ordered|factor|target)\\."), "", colnames(result))
      }
    } else {
      assert(whichfun == "invert")
      if (convertto != "multilabel" && predict.type == "response") {
        assert(ncol(result) == 1)
        result = result[[1]]
      }
    }

    result
  }

  convertDF = function(oldfun, whichfun) {
    if (is.null(oldfun)) {
      return(NULL)
    }
    function(...) {
      indata = list(...)
      convdata = reduceDataformat(indata$data, indata$target, whichfun)
      for (pn in c("data", "target")) {
        if (pn %in% names(indata)) {
          indata[[pn]] = convdata[[pn]]
        }
      }
      result = do.call(oldfun, indata)
      if (whichfun %in% c("retrafo", "invert")) {
        result = addDataformat(result, indata$data, indata$target, whichfun, predict.type)
      }
      result
    }
  }

  for (fun in c("train", "retrafo", "traininvert", "invert")) {
    assign(fun, convertDF(get(fun), fun))
  }

  target = "NOTGIVEN"

  switch(type,
    retrafoless = makeCPORetrafoless(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
      cpo.retrafo = {
        assert(target == "NOTGIVEN")
        retrafo(data = data, param = param,
          control = train(data = data, target = NULL, param = param))
      }),
    simple = {
      if (sl) {
        assert(!fr)
        makeCPO(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered, cpo.train = NULL,
          cpo.retrafo = {
            assert(target == "NOTGIVEN")
            retrafo(data = data, param = param,
              control = train(data = data, target = NULL, param = param))
          })
      } else if (fr) {
        makeCPO(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered, cpo.retrafo = NULL,
          cpo.train = {
            control = train(data = data, target = target, param = param)
            function(data) {
              retrafo(data = data, param = param, control = control)
            }
          })
      } else {
        makeCPO(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
          cpo.train = {
            train(data = data, target = target, param = param)
          }, cpo.retrafo = {
            assert(target == "NOTGIVEN")
            retrafo(data = data, param = param, control = control)
          })
      }
    },
    extended = {
      if (fr) {
        makeCPOExtendedTrafo(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered, cpo.retrafo = NULL,
          cpo.trafo = {
            control = train(data = data, target = target, param = param)
            cpo.retrafo = function(data) {
              retrafo(data = data, param = param, control = control)
            }
            cpo.retrafo(data)
          })
      } else {
        makeCPOExtendedTrafo(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
          cpo.trafo = {
            control = train(data = data, target = target, param = param)
            retrafo(data = data, param = param, control = control)
          },
          cpo.retrafo = {
            assert(target == "NOTGIVEN")
            retrafo(data = data, param = param, control = control)
          })
      }
    },
    target = {
      if (fi) {
        assert(!ci)
        assert(!sl)
        if (fr) {
          # fi, fr
          makeCPOTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
            cpo.retrafo = NULL, cpo.train.invert = NULL, cpo.invert = NULL,
            cpo.train = {
              control = train(data = data, target = target, param = param)
              cpo.retrafo = function(data, target) {
                retrafo(data = data, target = target, param = param, control = control)
              }
              cpo.train.invert = function(data) {
                control.invert = traininvert(data = data, control = control, param = param)
                function(target, predict.type) {
                  invert(target = target, predict.type = predict.type, control = control.invert, param = param)
                }
              }
            })
        } else { or
          # fi, or
          makeCPOTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
            cpo.train = {
              train(data = data, target = target, param = param)
            },
            cpo.retrafo = {
              retrafo(data = data, target = target, param = param, control = control)
            },
            cpo.train.invert = {
              control.invert = traininvert(data = data, control = control, param = param)
              function(target, predict.type) {
                invert(target = target, predict.type = predict.type, control = control.invert, param = param)
              }
            })
        }
      } else { # oi
        if (fr) {
          assert(!sl)
          if (ci) {
            # (oi), fr, constant invert
            makeCPOTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered, constant.invert = TRUE,
              cpo.train.invert = NULL, cpo.invert = NULL, cpo.retrafo = NULL,
              cpo.train = {
                control = train(data = data, target = target, param = param)
                cpo.retrafo = function(data, target) {
                  retrafo(data = data, target = target, param = param, control = control)
                }
                if (dataformat == "df.all") {
                  data = dropNamed(data, target)
                }
                control.invert = traininvert(data = data, control = control, param = param)
                cpo.invert = function(target, predict.type) {
                  invert(target = target, predict.type = predict.type, control = control.invert, param = param)
                }
              })
          } else {
            # oi, fr
            makeCPOTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
              cpo.retrafo = NULL, cpo.train.invert = NULL,
              cpo.train = {
                control = train(data = data, target = target, param = param)
                cpo.retrafo = function(data, target) {
                  retrafo(data = data, target = target, param = param, control = control)
                }
                cpo.train.invert = function(data) {
                  traininvert(data = data, control = control, param = param)
                }
              },
              cpo.invert = {
                invert(target = target, predict.type = predict.type, control = control.invert, param = param)
              })
          }
        } else {  # or
          if (ci) {
            if (sl) {
              # oi, or, const invert, stateless
              makeCPOTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered, constant.invert = TRUE,
                cpo.train = NULL, cpo.train.invert = NULL,
                cpo.retrafo = {
                  control = train(data = data, target = target, param = param)
                  retrafo(data = data, target = target, param = param, control = control)
                },
                cpo.invert = {
                  invert(target = target, predict.type = predict.type, control = NULL, param = param)
                })
            } else {
              # oi, or, const invert
              makeCPOTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered, constant.invert = TRUE,
                cpo.train.invert = NULL,
                cpo.train = {
                  control = train(data = data, target = target, param = param)
                  if (dataformat == "df.all") {
                    data = dropNamed(data, target)
                  }
                  list(control = control,
                    control.invert = traininvert(data = data, control = control, param = param))
                },
                cpo.retrafo = {
                  retrafo(data = data, target = target, param = param, control = control$control)
                },
                cpo.invert = {
                  invert(target = target, predict.type = predict.type, control = control.invert$control.invert, param = param)
                })
            }
          } else {
            assert(!sl)
            # oi, or
            makeCPOTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered, constant.invert = TRUE,
              cpo.train = {
                train(data = data, target = target, param = param)
              },
              cpo.train.invert = {
                traininvert(data = data, control = control, param = param)
              },
              cpo.retrafo = {
                retrafo(data = data, target = target, param = param, control = control)
              },
              cpo.invert = {
                invert(target = target, predict.type = predict.type, control = control.invert, param = param)
              })
          }
        }
      }
    },
    target.extended = {
      if (fi) {
        if (fr) {
          # fi, fr
          makeCPOExtendedTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
            constant.invert = ci,
            cpo.retrafo = NULL, cpo.invert = NULL,
            cpo.trafo = {
              control = train(data = data, target = target, param = param)
              if (dataformat == "df.all") {
                data2 = dropNamed(data, target)
              }
              control.invert = traininvert(data = data2, control = control, param = param)
              cpo.invert = function(target, predict.type) {
                invert(target = target, predict.type = predict.type, control = control.invert)
              }
              cpo.retrafo = function(data, target) {
                control.invert = traininvert(data = data, control = control, param = param)
                cpo.invert = function(target, predict.type) {
                  invert(target = target, predict.type = predict.type, control = control.invert)
                }
                retrafo(data = data, target = target, param = param, control = control)
              }
              retrafo(data = data, target = target)
            })

        } else { # or
          # fi, or
          makeCPOExtendedTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
            constant.invert = ci,
            cpo.invert = NULL,
            cpo.trafo = {
              control = train(data = data, target = target, param = param)
              if (dataformat == "df.all") {
                data2 = dropNamed(data, target)
              }
              control.invert = traininvert(data = data2, control = control, param = param)
              cpo.invert = function(target, predict.type) {
                invert(target = target, predict.type = predict.type, control = control.invert)
              }
              retrafo(data = data, target = target, param = param, control = control)
            },
            cpo.retrafo = {
              control.invert = traininvert(data = data, control = control, param = param)
              cpo.invert = function(target, predict.type) {
                invert(target = target, predict.type = predict.type, control = control.invert)
              }
              retrafo(data = data, target = target, param = param, control = control)
            })
        }
      } else {  # oi
        if (fr) {
          # oi, fr
          makeCPOExtendedTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
            constant.invert = ci,
            cpo.retrafo = NULL,
            cpo.trafo = {
              control = train(data = data, target = target, param = param)
              if (dataformat == "df.all") {
                data2 = dropNamed(data, target)
              }
              control.invert = traininvert(data = data2, control = control, param = param)
              cpo.retrafo = function(data, target) {
                control.invert = traininvert(data = data, control = control, param = param)
                retrafo(data = data, target = target, param = param, control = control)
              }
              cpo.retrafo(data = data, target = target)
            },
            cpo.invert = {
              invert(target = target, predict.type = predict.type, control = control.invert)
            })
        } else { # or
          # oi, or
          makeCPOExtendedTargetOp(name, ps, dataformat = dataformat, dataformat.factor.with.ordered = dataformat.factor.with.ordered,
            constant.invert = ci,
            cpo.trafo = {
              control = train(data = data, target = target, param = param)
              if (dataformat == "df.all") {
                data2 = dropNamed(data, target)
              }
              control.invert = traininvert(data = data2, control = control, param = param)
              retrafo(data = data, target = target, param = param, control = control)
            },
            cpo.retrafo = {
              control.invert = traininvert(data = data, control = control, param = param)
              retrafo(data = data, target = target, param = param, control = control)
            },
            cpo.invert = {
              invert(target = target, predict.type = predict.type, control = control.invert)
            })

        }  # if (fr)
      }  # if (or)
    })  # target.extended, switch
}
#  fr = FALSE, fi = FALSE, sl = FALSE, ci = FALSE
