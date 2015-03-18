# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

#' Dependency injection framework
#'
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom devtools install_version
#' @importFrom devtools install_svn
#' @importFrom devtools install_github
#' @importFrom devtools with_libpaths
#' @author levk
#' @docType package
#' @name raven
NULL;

.onLoad <- function (...) {
  op.raven <- list (raven.repo = 'http://raven-repository.appspot.com', raven.local = '~/.raven');

  toset <- !(names (op.raven) %in% names (options ()));
  if (any (toset)) options (op.raven[ toset ]);

  invisible ();
};

#' Provides the environment with library versions specified and runs the
#' code in that environment
#' 
#' @param ... library names and versions in named list to string format
#' (e.g. limma='3.22.7'), if the version is ommitted (the library is
#' still the named parameter with no value) a warning is issued listing
#' available versions and the version used
#' @param code to run in the provisioned environment, if omitted returns
#' the library path of provisioned environment
#' @param local the local root directory for the installed packages, if
#' omitted uses the option 'raven.local'
#' @param repo the repository address, if omitted uses the option 
#' 'raven.repo'
#' @return return of the code evaluated within the environment
#' @export 
#' @examples
#' provide (NMF = '0.20.5', code = print (.libPaths ()));
#' provide (NMF =, code = print (.libPaths ());
provide <- function (..., code = invisible (.libPaths ()), local = getOption ('raven.local'), repo = getOption ('raven.repo')) {
  packages <- match.call (expand.dots = FALSE)$...;
  devtools::with_libpaths (unique (unlist (sapply (names (packages),
                                           function (name) (
                                             install <- function (name, version) {
                                               if (version == '')
                                                 tryCatch ({
                                                   versions <- jsonlite::fromJSON (paste (repo, 'repository', name, sep = '/'));
                                                   version <- versions[ 1 ];
                                                   warning (paste ("No version specified for", name, "using",
                                                                   version, "from available", paste (versions, collapse = ', ')),
                                                            call. = FALSE);
                                                 }, error = function (e) stop (paste ("Could not obtain version information for", name, e)));
                                               tryCatch (info <- jsonlite::fromJSON (paste (repo, 'repository', name, version, sep = '/')),
                                                         error = function (e) stop (paste ("Unable to resolve", paste (name, version, sep = ':'), e)));
                                               deps <- c (list (), info$depends, info$imports, info$linksTo);
                                               path <- file.path (local, 'repository', name, version, paste ('.r', eval (expression (version$`svn rev`),
                                                                                                             baseenv ()), sep = ''));
                                               installed <- file.exists (path);
                                               if (!installed) dir.create (path, recursive = TRUE);
                                               paths <- c (path, if (length (deps) > 0) unlist (lapply (1:length (deps),
                                                                                                        function (i)
                                                                                                          install (names (deps)[ i ], deps[[ i ]])))
                                                                 else NULL);
                                               devtools::with_libpaths (paths,
                                                                        tryCatch ({
                                                                          if (!installed)
                                                                            switch (info$`@c`,
                                                                                    .Cran = devtools::install_version (package = name, version = version),
                                                                                    .Svn = devtools::install_svn (url = info$url, subdir = info$dir,
                                                                                                                  branch = info$branch, revision = info$revision,
                                                                                                                  args = if (is.null (info$user)) ''
                                                                                                                         else paste ('--username', info$user,
                                                                                                                                     '--password', info$pw)),
                                                                                    .Github = devtools::install_github (repository = info$repo,
                                                                                                              ref = info$sha));
                                                                          paths;
                                                                        }, error = function (e) {
                                                                          unlink (path, recursive = TRUE);
                                                                          stop (e);
                                                                        }));
                                   }) (name, packages[[ name ]])))), force (code));
};
