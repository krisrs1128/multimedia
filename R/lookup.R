#' Variables in a Multimedia Object
#'
#' This returns all the variables modeled within a multimedia object. This can
#' be helpful for overviewing an experiment and is called by the print methods
#' in this package. Also supports filtering to specific node types, e.g.,
#' mediators.
#'
#' @param object An object of class multimedia
#' @param nm A string specifying the node type to filter down to, e.g.,
#'   'treatment' or 'mediator'.
#' @return A character vector containing all the names of the variables of the
#'   node type `nm`.
#' @importFrom rlang .data
#' @importFrom dplyr filter pull
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     retrieve_names("mediator")
#' @export
retrieve_names <- function(object, nm) {
    object@edges |>
        filter(.data$node_type == nm) |>
        pull(.data$name)
}

#' Graphical Structure for Mediation Objects
#'
#' We often want to access the DAG for different mediation-related S4 objects.
#' This generic helps us access these graphical model edges lists. See method
#' instantiations for specific examples.
#' @param object An object whose DAG structure we want to access.
#' @return The output depends on the S4 object that is passed. For multimedia
#'   objects, this will return an edgelist as a two column data.frame.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     edges()
#' @importFrom methods setGeneric
#' @export
setGeneric("edges", \(object) standardGeneric("edges"))

#' Access Class-Specific Mediators
#'
#' This is a shorthand for accessing mediator-related slots in classes exported
#' by the multimedia package.
#'
#' @param object An object whose mediators we want to access (either their names
#'   or values).
#' @return The output will depend on the class of the object that is passed in.
#'   For multimedia objects, this returns a character vector of mediators. For
#'   mediation data objects, this returns the mediator mediator data.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' mediators(exper)
#' @export
setGeneric("mediators", \(object) standardGeneric("mediators"))

#' Set Mediators
#'
#' This is an setter method for mediators in an S4 object, usually of class
#' mediation_data.
#' @param object An object whose mediators slot to modify.
#' @param value The new mediator values to set within object.
#' @return Modifies the mediators slot of the input object in place.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' mediators(exper) <- data.frame(new_ASV = rnorm(100))
#' mediators(exper)
#' @export
setGeneric("mediators<-", \(object, value) standardGeneric("mediators<-"))

#' Access Outcomes
#'
#' This is an getter method for outcomes in an S4 object, usually of class
#' mediation_data.
#' @param object An object whose outcomes slot to modify.
#' @return The output depends on the S4 class of the input object. If it is a
#'   multimedia model object, it will return a character vector of the outcome
#'   variable names. Otherwise it returns the outcome data.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' outcomes(exper)
#' @export
setGeneric("outcomes", \(object) standardGeneric("outcomes"))

#' Set Outcomes
#' This is an setter method for outcomes in an S4 object, usually of class
#' mediation_data.
#' @param object An object whose outcomes slot to modify.
#' @param value The new outcome values to set within object.
#' @return Modifies the outcomes slot of the input object in place.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' outcomes(exper) <- data.frame(new_PHQ = rnorm(100))
#' @export
setGeneric("outcomes<-", \(object, value) standardGeneric("outcomes<-"))

#' Access Pretreatments
#'
#' This is an getter method for pretreatments in an S4 object, usually of class
#' mediation_data.
#' @param object An object whose pretreatments slot to modify.
#' @return The output depends on the S4 class of the input object. If it is a
#'   multimedia model object, it will return a character vector of the outcome
#'   variable names. Otherwise it returns the outcome data.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' pretreatments(exper)
#' @export
setGeneric("pretreatments", \(object) standardGeneric("pretreatments"))

#' Set Pretreatments
#' This is an setter method for pretreatments in an S4 object, usually of class
#' mediation_data.
#' @param object An object whose pretreatments slot to modify.
#' @param value The new pretreatment values to set within object.
#' @return Modifies the pretreatments slot of the input object in place.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' pretreatments(exper) <- data.frame(new_var = rnorm(100))
#' @export
setGeneric("pretreatments<-", \(object, value) {
    standardGeneric("pretreatments<-")
})

#' Access Treatments
#'
#' This is an getter method for treatments in an S4 object, usually of class
#' mediation_data.
#' @param object An object whose treatments slot to modify.
#' @return The output depends on the S4 class of the input object. If it is a
#'   multimedia model object, it will return a character vector of the outcome
#'   variable names. Otherwise it returns the outcome data.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' treatments(exper)
#' @export
setGeneric("treatments", \(object) standardGeneric("treatments"))

#' Set Treatments
#' This is an setter method for treatments in an S4 object, usually of class
#' mediation_data.
#' @param object An object whose treatments slot to modify.
#' @param value The new treatment values to set within object.
#' @return Modifies the treatments slot of the input object in place.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' treatments(exper) <- data.frame(new_treatment = rnorm(100))
#' @export
setGeneric("treatments<-", \(object, value) standardGeneric("treatments<-"))

#' Names of Mediators in a Multimedia Object
#'
#' This is a helper that returns the names of the mediator variables in an
#' object of class multimedia. It parses the graph in the DAG specifying the
#' mediation analysis, and it returns all variables between treatment and
#' outcome.
#'
#' @param object An object of class multimedia.
#' @return m A vector of strings containing the names of all the mediators.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     mediators()
#' @importFrom methods setMethod
#' @export
setMethod(
    "mediators", "multimedia",
    \(object) {
        retrieve_names(object, "mediator")
    }
)

#' Access to @mediators in Mediation Data
#'
#' This is an accessor returns the @mediators slot in a mediation_data object.
#'
#' @param object An object of class mediation_data.
#' @return m A data.frame whose rows are samples and columns are values of
#'   mediators across those samples.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' mediators(exper)
#' @export
setMethod(
    "mediators", "mediation_data",
    \(object) {
        object@mediators
    }
)

#' Names of Outcomes in a Multimedia Object
#'
#' This is a helper that returns the names of the outcome variables in an
#' object of class multimedia. It parses the graph in the DAG specifying the
#' mediation analysis, and it returns all variables of node type outcome.
#'
#' @param object An object of class multimedia.
#' @return m A vector of strings containing the names of all the outcomes.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     outcomes()
#'
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper) |>
#'     outcomes()
#' @export
setMethod(
    "outcomes", "multimedia",
    \(object) {
        retrieve_names(object, "outcome")
    }
)

#' Outcomes Data in a Mediation Data Object
#'
#' This is an accessor function to the @outcomes slot in a mediation data
#' object. It returns the entire outcomes dataset, in contrast to outcomes()
#' applied to a multimedia object, which only returns the names of the outcome
#' variables.
#'
#' @param object An object of class mediation_data.
#' @return A data.frame whose rows are samples and columns different outcomes.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' outcomes(exper)
#' @export
setMethod(
    "outcomes", "mediation_data",
    \(object) {
        object@outcomes
    }
)

#' Treatments in a Mediation Data Object
#'
#' This is an accessor function to the @treatments slot in a mediation data
#' object. It returns the entire set of observed treatments, in contrast to
#' treatments() applied to a multimedia object, which only returns the names of
#' the treatment variables.
#'
#' @param object An object of class mediation_data.
#' @return A data.frame whose rows are samples and columns different treatments.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' treatments(exper)
#' @export
setMethod(
    "treatments", "mediation_data",
    \(object) {
        object@treatments
    }
)

#' Pretreatments in a Mediation Data Object
#'
#' This is an accessor function to the @pretreatments slot in a mediation data
#' object. It returns the entire set of observed pretreatments, in contrast to
#' pretreatments() applied to a multimedia object, which only returns the names
#' of the pretreatment variables.
#'
#' @param object An object of class mediation_data.
#' @return A data.frame whose rows are samples and columns different
#'   pretreatments.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' pretreatments(exper)
#' @export
setMethod(
    "pretreatments", "mediation_data",
    \(object) {
        object@pretreatments
    }
)

#' Set the Outcomes in a Mediation Data Object
#'
#' This is an setter method for the outcomes slot in a mediation data object. It
#' lets you supply a new outcomes data.frame for the object.
#'
#' @param object An object of class mediation_data.
#' @param value The new outcome values for the object.
#' @return A version of `object` whose outcomes slot has been replaced.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' outcomes(exper) <- data.frame(y = 1:10)
#' exper
#' @export
setMethod(
    "outcomes<-", "mediation_data",
    \(object, value) {
        object@outcomes <- value
        object
    }
)

#' Set the Mediators in a Mediation Data Object
#'
#' This is an setter method for the mediators slot in a mediation data object.
#' It lets you supply a new mediators data.frame for the object.
#'
#' @param object An object of class mediation_data.
#' @param value The new mediators values for the object.
#' @return A version of `object` whose mediators slot has been replaced.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' mediators(exper) <- data.frame(m = 1:10)
#' exper
#' @export
setMethod(
    "mediators<-", "mediation_data",
    \(object, value) {
        object@mediators <- value
        object
    }
)

#' Set the Treatments in a Mediation Data Object
#'
#' This is an setter method for the treatments slot in a mediation data object.
#' It lets you supply a new treatments data.frame for the object.
#'
#' @param object An object of class mediation_data.
#' @param value The new treatment values for the object.
#' @return A version of `object` whose treatment slot has been replaced.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' treatments(exper) <- data.frame(t = rep(0, 10))
#' exper
#' @export
setMethod(
    "treatments<-", "mediation_data",
    \(object, value) {
        object@treatments <- value
        object
    }
)

#' Set the Pretreatments in a Mediation Data Object
#'
#' This is an setter method for the pretreatments slot in a mediation data
#' object. It lets you supply a new pretreatments data.frame for the object.
#'
#' @param object An object of class mediation_data.
#' @param value The new pretreatment values for the object.
#' @return A version of `object` whose pretreatments slot has been replaced.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' pretreatments(exper) <- data.frame(x = 1:10)
#' exper
#' @export
setMethod(
    "pretreatments<-", "mediation_data",
    \(object, value) {
        object@pretreatments <- value
        object
    }
)

#' Access Mediation Model DAG
#'
#' This is an accessor to the edges slot in a multimedia object. It is the
#' internal representation of the variable conditional dependence graph encoded
#' by the mediation model's DAG.
#'
#' @param object An object of class multimedia.
#' @return A data.frame whose rows give edges in the mediation analysis DAG.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     edges()
#' @export
setMethod(
    "edges", "multimedia",
    \(object) {
        object@edges
    }
)


#' Names of Treatments in a Multimedia Object
#'
#' This is a helper that returns the names of the treatment variables in an
#' object of class multimedia. It parses the graph in the DAG specifying the
#' mediation analysis, and it returns all variables of node type treatment.
#'
#' @param object An object of class multimedia.
#' @return m A vector of strings containing the names of all the treatments.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     treatments()
#' @export
setMethod(
    "treatments", "multimedia",
    function(object) {
        retrieve_names(object, "treatment")
    }
)

#' Number of Mediators in a Multimedia Object
#'
#' @param object An object of class multimedia.
#' @return An integer specifying the number of mediators.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     n_mediators()
#' @export
n_mediators <- function(object) {
    length(mediators(object))
}

#' Number of Outcomes in a Multimedia Object
#'
#' @param object An object of class multimedia.
#' @return An integer specifying the number of outcomes.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     n_outcomes()
#' @export
n_outcomes <- function(object) {
    length(outcomes(object))
}

#' Access the Outcome Model in a Multimedia Object
#'
#' This is an accessor to the outcome slot of a multimedia object.
#' @param object An object of class multimedia.
#' @return NULL, if not fitted, or the model learned from the training mediation
#'   data object. For models fit in parallel across outcomes (e.g.,
#'   glmnet_model()) a list of separate model objects. For models fitted jointly
#'   across outcomes (e.g., lnm_model()), a single model object of that class.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     outcome_model()
#' @export
outcome_model <- function(object) {
    object@outcome
}
