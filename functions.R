#' Predictive model ``blinded'' from protected covariates.
#'
#' Calls \code{\link{lm}} or \code{\link{e1071::svm}},
#' omitting the protected variables.
#' Assumes outcome is centered.
#'
#' @param data data.frame containing all relevant variables
#' @param res response variable name
#' @param prot name(s) of protected variables
#' @param method one of "lm" or "svm" specifying prediction algorithm
#'
#' @return Model object of type "lm" or "svm"
#'
#' @export
fairpred_blind <- function(data, res, prot, method, ...) {
  form <- formula(paste(res, "~ . -1 -", paste(prot, collapse = "-")))
  return(eval(call(method, form, data = data)))
}

#' Two-stage predictive modeling procedure for protected covariates.
#'
#' Calls \code{\link{lm}} or \code{\link{e1071::svm}} to regress
#' out protected covariates, then fits on residuals.
#' Assumes outcome is centered.
#'
#' @param data data.frame containing all relevant variables
#' @param res response variable name
#' @param prot name(s) of protected variables
#' @param method1 one of "lm" or "svm" specifying orthogonality algorithm
#' @param method2 one of "lm" or "svm" specifying prediction algorithm
#'
#' @return Model object of type determined by method2
#'
#' @export
fairpred_2s <- function(data, res, prot, method1, method2, ...) {
  form1 <- formula(paste(res, "~ -1 +", paste(prot, collapse = "+")))
  m1 <- eval(call(method1, form1, data = data))
  data2 <- data
  data2[,res] <- resid(m1)
  form2 <- formula(paste(res, "~ . -1 -", paste(prot, collapse = "-")))
  return(eval(call(method2, form2, data = data2)))
}

#' Predictive model penalizing all non-protected coefficients.
#'
#' Calls \code{\link{cv.glmnet}}
#' with a penalty factor of 0 for protected variables.
#' Assumes outcome is centered.
#'
#' @param data data.frame containing all relevant variables
#' @param res response variable name
#' @param prot name(s) of protected variables
#'
#' @return List containing model object from cv.glmnet and design matrix.
#'
#' @export
fairpred_pen <- function(data, res, prot, alpha = 0) {
  protx <- model.matrix(formula(paste("~-1+", paste(prot, collapse = "+"))), data)
  unprotx <- model.matrix(formula(paste(res, "~ . -1 -", paste(prot, collapse = "-"))), data)
  x <- cbind(protx, unprotx)
  y <- data[,res]
  model <- cv.glmnet(x, y, alpha = alpha, intercept = FALSE,
                    penalty.factor = c(rep(0, ncol(protx)),
                                       rep(1, ncol(unprotx))))
  coefs <- coef(model, newx=x, s = "lambda.min")[-1,1]
  predictions <- unprotx %*% coefs[-(1:ncol(protx))] + sum(colMeans(protx) * coefs[1:ncol(protx)])
  return(list(model = model, predictions = predictions[,1], coefs = coefs, x=x))
}
