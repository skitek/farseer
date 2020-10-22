# Copyright (C) 2020 Kornel Skitek
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#'farseer (R package)
#'
#' @import "stats"
#' @concept 
#' farseer is a proof-of-concept package for easy machine learning
#' for clinicians.
#' 
#' It creates three models - linear, partition tree and neural network
#' for each dependand variable specified by calling \link{farseer} function.
#' Those models are automatically evaluated using bland-altman or roc curves, 
#' depending of the type of the target (numeric or factor). 
#' Please note that only two-level factors are currently supported.
#' Those evaluations can be visualized using generic plot() function.
#' 
#' An S3 farseer-class object is then created, which can be used for simulation
#' on new patient data.
#' 
#' The \link{farseer.simulate} does exactly that, it takes patient data, then simulates a variable
#' (for example: height, age or ASA score) and for all combinations of those
#' simulated variables calculates dependent variables using previously trained models.
#' 
#' @section DISCLAIMER:
#' 
#' Copyright (C) 2020 Kornel Skitek
#' 
#' This software was developed for research purposes, and its output
#' should be used with extreme care in clinical situations.
#' All calculated values should be interpreted within the boundaries
#' and limitations of trained models.
#' 
#' Under no circumstances is this software a medical product and 
#' no warranty is provided. See license for more details.
#' 
#' additionally, a standard disclaimer from GNU-GPL v. 3.0 applies:
#'
#' This program is free software: you can redistribute it and/or modify
#' it under the terms of the GNU General Public License as published by
#' the Free Software Foundation, either version 3 of the License, or
#' (at your option) any later version.

#' This program is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#' GNU General Public License for more details.

#' You should have received a copy of the GNU General Public License
#' along with this program.  If not, see <http://www.gnu.org/licenses/>.
#' 
#' @author Kornel Skitek (C) 2020
#' @docType package
#' @name farseer
NULL
#> NULL