# Changes of tests assertive of API integrity must necessitate bump in major version

describe ("Options", {
  it ("Should declare 'raven.local' option", expect_true (is.character (getOption ('raven.local'))));
  
  it ("Should declare 'raven.repo' option", expect_true (is.character (getOption ('raven.repo'))));
});

describe ("Provision facility", {
  it ("Should be a function accepting ..., code, local, and repo parameters",
      expect_equal (names (formals (provide)), c ('...', 'code', 'local', 'repo')));

  it ("Should fetch version information and install from cran in correct folder", {
    with_mock (`jsonlite::fromJSON` = (function () {
                 d <- list (list (url = paste (getOption ('raven.repo'), 'repository/foo', sep = '/'),
                                  result = c ('0.1', '0.0.99', '99.0.0')),
                            list (url = paste (getOption ('raven.repo'), 'repository/foo/0.1', sep = '/'),
                                  result = list (`@c` = '.Cran', package = 'foo', version = '0.1')));
                 called <- 0;
                 function (txt) {
                   called <<- called + 1;
                   expect_equal (d[[ called ]]$url, txt);
                   d[[ called ]]$result;
                 };
               }) (), `devtools::with_libpaths` = function (new, code) {
                 expect_equal (new, file.path (getOption ('raven.local'), 'repository', 'foo', '0.1', paste ('.r', eval (expression (version$`svn rev`), baseenv ()), sep = '')));
                 force (code);
               }, `base::dir.create` = function (path, recursive) expect_true (recursive),
               `devtools::install_version` = function (package, version) {
                 expect_equal (package, 'foo');
                 expect_equal (version, '0.1');
               }, expect_warning (provide (foo =)));
  });

  it ("Should fetch version information and install from svn in correct folder", {
    with_mock (`jsonlite::fromJSON` = (function () {
                 d <- list (list (url = paste (getOption ('raven.repo'), 'repository/foo', sep = '/'),
                                  result = c ('0.1', '0.0.99', '99.0.0')),
                            list (url = paste (getOption ('raven.repo'), 'repository/foo/0.1', sep = '/'),
                                  result = list (`@c` = '.Svn', dir = 'dir', revision = 33, url = 'svn://root', user = 'u', pw = 'p')));
                 called <- 0;
                 function (txt) {
                   called <<- called + 1;
                   expect_equal (d[[ called ]]$url, txt);
                   d[[ called ]]$result;
                 };
               }) (), `devtools::with_libpaths` = function (new, code) {
                 expect_equal (new, file.path (getOption ('raven.local'), 'repository', 'foo', '0.1', paste ('.r', eval (expression (version$`svn rev`), baseenv ()), sep = '')));
                 force (code);
               }, `base::dir.create` = function (path, recursive) expect_true (recursive),
               `devtools::install_svn` = function (url, subdir, revision, branch, args) {
                 expect_equal (url, 'svn://root');
                 expect_equal (subdir, 'dir');
                 expect_true (is.null (branch));
                 expect_equal (revision, 33);
                 expect_equal (args, '--username u --password p');
               }, expect_warning (provide (foo =)));
  });

  it ("Should fetch version information and install from github in correct folder", {
    with_mock (`jsonlite::fromJSON` = (function () {
                 d <- list (list (url = paste (getOption ('raven.repo'), 'repository/foo', sep = '/'),
                                  result = c ('0.1', '0.0.99', '99.0.0')),
                            list (url = paste (getOption ('raven.repo'), 'repository/foo/0.1', sep = '/'),
                                  result = list (`@c` = '.Github', repo = 'hello/world', sha = 'eee')));
                 called <- 0;
                 function (txt) {
                   called <<- called + 1;
                   expect_equal (d[[ called ]]$url, txt);
                   d[[ called ]]$result;
                 };
               }) (), `devtools::with_libpaths` = function (new, code) {
                 expect_equal (new, file.path (getOption ('raven.local'), 'repository', 'foo', '0.1', paste ('.r', eval (expression (version$`svn rev`), baseenv ()), sep = '')));
                 force (code);
               }, `base::dir.create` = function (path, recursive) expect_true (recursive),
               `devtools::install_github` = function (repo, ref) {
                 expect_equal (repo, 'hello/world');
                 expect_equal (ref, 'eee');
               }, expect_warning (provide (foo =)));
  });
});
