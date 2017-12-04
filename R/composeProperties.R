# calculate the properties, properties.adding and properties.needed for a composite CPO
# CPO1 %>>% CPO2
# If the properties don't match (e.g. CPO1 'needs' a property that CPO2 does not support), an error is thrown.
# @param prop1 [list of character] properties of CPO1, with slots "properties", "properties.adding", and "properties.needed"
# @param prop2 [list of character] properties of CPO2, with same slots as `prop1`
# @name1 [character(1)] name of CPO1 to print in debug messages
# @name2 [character(1)] name of CPO2 to print in debug messages
# @return [list of character] properties of compound CPO, with same slots as `prop1`
# returns a list(handling, adding, needed)
composeProperties = function(prop1, prop2, name1, name2) {
  handling.1 = prop1$handling
  adding.1 = prop1$adding
  needed.1 = prop1$needed
  adding.min.1 = prop1$adding.min
  needed.max.1 = prop1$needed.max

  handling.2 = prop2$handling
  adding.2 = prop2$adding
  needed.2 = prop2$needed
  adding.min.2 = prop2$adding.min
  needed.max.2 = prop2$needed.max

  assertCharacter(handling.1, unique = TRUE)
  assertCharacter(handling.2, unique = TRUE)
  assertCharacter(adding.1, unique = TRUE)
  assertCharacter(adding.2, unique = TRUE)
  assertCharacter(adding.min.1, unique = TRUE)
  assertCharacter(adding.min.2, unique = TRUE)
  assertCharacter(needed.1, unique = TRUE)
  assertCharacter(needed.2, unique = TRUE)
  assertCharacter(needed.max.1, unique = TRUE)
  assertCharacter(needed.max.2, unique = TRUE)

  assertString(name1)
  assertString(name2)
  # some explanation about properties:
  # * 'handling' are the properties that a CPO can handle.
  # * 'adding' are the properties it adds to the things coming after it, it is therefore
  #   the things it *removes* from a dataset. E.g. if it removes 'missings' from data, it adds the property
  #   'missings' to the pipeline.
  # * needed are the properties it needs from the things coming after it, this are the
  #   the things it *adds* to a dataset. E.g. if it converts numerics to factors, it 'needs' the learner /
  #   CPOs coming after it to have the property 'factors'.

  # The conditions on the properties are:
  # A) adding is a subset of handling
  # B) adding and needed have no common elements
  # (these should be checked upon creation of a CPO)

  # When composing two CPOs (CPO1 %>>% CPO2), there is an additional requirement:
  # * needed.1 is a subset of handling.2
  missing.properties = setdiff(needed.1, handling.2)
  if (isPropertyStrict() && length(missing.properties)) {
    stopf("CPO %s creates data with propert%s %s that %s can not handle.",
      name1, ifelse(length(missing.properties) > 1, "ies", "y"),
      collapse(missing.properties, sep = ", "),
      name2)
  }

  # The properties of the new CPO are obtained thus:
  composite = intersect(handling.1, union(handling.2, adding.1))
  adding.composite = union(setdiff(adding.1, needed.2), intersect(handling.1, adding.2))
  needed.composite = union(setdiff(needed.1, adding.2), needed.2)

  adding.min.composite = union(setdiff(adding.min.1, needed.max.2), intersect(handling.1, adding.min.2))
  needed.max.composite = union(setdiff(needed.max.1, adding.min.2), needed.max.2)


  # Proofs that conditions (A) and (B) are still fulfilled:
  # A) using distribution of union and intersect, and the fact that (cond (A)) intersect(adding.1, handling.1) == adding.1,
  #    we rewrite
  #      composite = union(adding.1, intersect(handling.1, handling.2))
  #    Now the first term in the union of adding.composite is a subset of the first term of the union of composite;
  #    same with the second term of both.
  # B) We show that intersect(adding.composite, needed.composite) is empty by showing that both terms in the union
  #    of adding.composite have empty intersect each with both terms in the union of needed.composite.
  #    1) intersect(adding.1 - needed.2, needed.2) is empty because needed.2 is subtracted from the lhs
  #    2) intersect(adding.1 - needed.2, needed.1 - adding.2) is empty because
  #       intersect(adding.1, needed.1) is empty per condition (B)
  #    3) intersect(intersect(handling.1, adding.2), needed.1 - adding.2) is empty because adding.2
  #       is subtracted from the rhs
  #    4) intersect(intersect(handling.1, adding.2), needed.2) is empty because needed.2 and adding.2
  #       have empty intersect per condition (B)
  list(handling = composite, adding = adding.composite, needed = needed.composite,
    adding.min = adding.min.composite, needed.max = needed.max.composite)
}
