.binder <- new.env (parent = emptyenv ());

binder <- function (parent = .binder, callback = function (binder) binder)
  callback (new.env (parent = parent));

default <- function (key, provider) provider;
    
eager <- function (key, provider) {
  value <- provider ();
  function () value;
};

define <- function (key, factory, scope = default, binder = .binder)
  binder[[ key ]] <- scope (key, function () inject (factory, binder));

override <- function (current, parent) current;

collection <- function (key, scope = default, combine = function (current, parent) c (current, parent ()), binder = .binder) {
  factories <- NULL;
  define (key, function () {
    collection <- list ();
    for (factory in factories) collection [[ length (collection) + 1 ]] <- inject (factory, binder);
    parent <- parent.env (binder);
    combine (collection, function () if (exists (key, e = parent)) get (key, e = parent) () else list ());
  }, scope, binder);
  function (factory) factories <<- c (factories, factory);
};

inject <- function (callback, binder = .binder, local = FALSE) {
  arguments <- list ();
  errors <- list ();

  for (key in names (formals (callback)))
    if (exists (key, e = binder, inherits = !local))
      tryCatch (arguments[[ key ]] <- get (key, e = binder) (), error = function (chain) errors <<- c (errors, chain));

  if (length (errors) == 0) do.call (callback, arguments) else stop (errors);
};

clean <- function () function (build) unlink (build, recursive = TRUE);
  
inspect <- function () function (binder, base, debug) {
  debug ("Inspecting", base)

  define ('build', function () file.path (base, 'target'), eager, binder);
  define ('sources', function () base, eager, binder);

  dependency <- collection ('dependencies', combine = override, b = binder);
  remote <- collection ('remote', b = binder);

  with (list (project = function (name, version, packaging) {
                define ('name', function () name, eager, binder);
                define ('version', function () version, eager, binder);
                for (name in ls (packaging, all.names = TRUE)) (function (key) define (key, packaging[[ key ]], b = binder)) (name);
              },
              modules = function (...)
                list (on.inspect = function (goals, base, binder, log, local) {
                        for (module in c (...))
                          raven (goals, base = file.path (base, module), parent = binder,
                                 restore.libPaths = FALSE, log = log, local = local);
                        'ok';
                      }),
              script = function (sources = '.', tests = NULL)
                list (.scripts = function () function (root, scripts)
                        vapply (Filter (function (script) "raven.R" != script, scripts),
                                function (x) file.path (root, x), c (''), USE.NAMES = FALSE),
                      .source = function (base) function (script, environment) {
                        debug (paste ("Sourcing", file.path (base, script)));
                        tryCatch (source (file.path (base, script), local = environment),
                                  error = function (e) stop (paste ("Error in", file.path (base, script),
                                                                     e$message, "in", paste (deparse (e$call), collapse=''))));
                      },
                      on.fetch = function (name, version, local) {
                        untar (file.path (local, name, version, paste (paste (name, version, sep = '_'), 'tar', 'gz', sep = '.')),
                               exdir = file.path (local, name, version), tar = 'tar');
                        'ok';
                      },
                      on.import = function (name, version, base, execution, .scripts, .source) {
                        for (script in .scripts (sources,
                                                 list.files (path = file.path (base, sources), pattern = '*\\.R$', recursive = TRUE)))
                          .source (script, execution);
                        'ok';
                      },
                      on.test = function (name, version, base, execution, .scripts, .source, info) if (!is.null (tests)) tryCatch ({
                        save <- getwd ();
                        setwd (base);
                        success <- 0;
                        failures <- 0;
                        report <- '';
                        spaces <- 0;
                        indent <- function (string) sprintf (paste ('%', nchar (string) + spaces, 's', sep = ''), string);
                        execution <- list2env (list (suite = function (suite, callback) {
                                                       report <<- paste (report, indent (suite), sep = '\n');
                                                       spaces <<- spaces + 2;
                                                       callback ();
                                                       spaces <<- spaces - 2;
                                                     },
                                                     it = function (component, callback) {
                                                       spaces <<- spaces + 2;
                                                       before <- failures;
                                                       callback ();
                                                       report <<- paste (report,
                                                                         indent (paste (component,
                                                                                        if (failures == before) '✔' else '✘')),
                                                                         sep = '\n');
                                                       spaces <<- spaces - 2;
                                                     },
                                                     expect = function (assertion) {
                                                       if (!assertion) failures <<- failures + 1 else success <<- success + 1;
                                                     }),
                                               parent = execution);
                        for (script in .scripts (tests, list.files (path = tests, pattern = '*\\.R$', recursive = TRUE)))
                          .source (script, execution);
                        info (report, '\nResults:', success + failures, 'specs,', failures, 'failures');
                        if (failures > 0) stop ("Assertion failures") else 'ok';
                      }, finally = setwd (save)),
                      on.install = function (name, version, base, local, .scripts, binder) if (!is.null (sources)) tryCatch ({
                        save <- getwd ();
                        setwd (base);
                        dir.create (file.path (local, name, version), recursive = TRUE, showWarnings = FALSE);
                        tar (file.path (local, name, version, paste (paste (name, version, sep = '_'), 'tar', 'gz', sep = '.')),
                             files = .scripts (sources, list.files (sources, patter = '*\\.R$', recursive = TRUE)),
                             tar = 'tar');
                        inject (function (on.fetch) on.fetch, binder, TRUE);
                      }, finally = setwd (save)) else 'not applicable'),
            cran = function ()
              list (.cran = function (name, version, local) 
                      file.path (local, name, version, paste ('.r', eval (expression (version$`svn rev`), baseenv ()), sep = '')),
                    on.fetch = function (name, version, local, .cran, binder)
                      if (!file.exists (.cran)) tryCatch ({
                        dir.create (.cran, recursive = TRUE);
                        install.packages (file.path (local, name, version, paste (paste (name, version, sep = '_'),
                                                                                  'tar', 'gz', sep = '.')),
                                          type = 'source', repo = NULL, lib = .cran);
                        inject (function (on.import) on.import, binder, TRUE);
                      }, error = function (error) {
                        unlink (.cran);
                        stop (error);
                      }),
                    on.import = function (.cran) {
                      if (!(.cran %in% .libPaths ())) .libPaths (c (.cran, .libPaths ()));
                      'ok';
                    },
                    on.test = function (on.import) on.import),
            set = function (key, value) define (key, function () value, b = binder),
            remote = function (url) remote (function () url),
            build = function (path) define ('build', function () path, eager, binder),
            dependency = function (name, version, required = TRUE)
              dependency (function () list (name = name, version = version, required = required))),
        source (file.path (base, 'raven.R'), local = TRUE));

  inject (function (info, debug, base, remote, dependencies, local, name, version) {
    info ("Project", paste (name, version, sep = ':'), "in", base);
    if (length (remote) > 0) debug ("Fetching remotely from", paste (remote, collapse = ', '));
    debug ("Installing locally into", local);
    if (length (dependencies) > 0)
      debug ("With dependencies on", paste (lapply (dependencies, function (d) paste (d$name, d$version, sep = ':')),
                                            collapse = ', '));
  }, binder);

  inject (function (on.inspect = 'ok') on.inspect, binder = binder, local = TRUE);
};

fetch <- function (force = FALSE) function (name, version, local, remote, dependencies, binder, info, debug) {
  debug ("Fetching dependencies for", paste (name, version, sep = ':'));
  debug ("Available remotes: ", paste (remote, collapse = ', '));
  for (dependency in dependencies)
    (function (name, version, required) {
      base <- file.path (local, name, version);
      archive <- paste (paste (name, version, sep = '_'), 'tar', 'gz', sep = '.');
      if (force) unlink (base, recursive = TRUE);
      if (!file.exists (base)) dir.create (base, recursive = TRUE, showWarnings = FALSE);
      if (!(file.exists (file.path (base, archive)) && file.exists (file.path (base, 'raven.R'))))
        for (root in remote) tryCatch ((function (...) for (file in c (...)) {
          to <- file.path (base, file);
          from <- paste (root, name, version, file, sep = '/');
          debug ("Downloading", from, "to", to);
          download.file (from, to, 'curl');
        }) ('raven.R', archive));
      if (file.exists (file.path (base, archive)) && file.exists (file.path (base, 'raven.R')))
        raven (inspect (), fetch (force), function (on.fetch = 'ok') on.fetch,
               base = base, restore.libPaths = FALSE, log = 'warn', local = local)
      else if (required) stop (paste ("Unable to fetch required dependency", paste (name, version, sep = ':'), '\n'));
    }) (dependency$name, dependency$version, dependency$required);
  debug ("Finished fetching dependencies for", paste (name, version, sep = ':'))
};

import <- function (execution) function (local, binder, dependencies, name, version, info) {
  info ("Importing", paste (name, version, sep = ':'));
  define ('execution', function () execution, b = binder);
  for (dependency in dependencies)
    (function (name, version, required)
      tryCatch (raven (inspect (), import (execution),
                       base = file.path (local, name, version), restore.libPaths = FALSE,
                       log = 'warn', local = local))) (dependency$name, dependency$version, dependency$required);
  inject (function (on.import = 'ok') on.import, binder, TRUE);
};

install <- function () function (binder, local, name, version, build, base, info) inject (function (on.install = 'not applicable') {
  if (on.install == 'ok') {
    info ("Installing", paste (name, version, sep = ':'))
    (function (...) tryCatch ({
      if (!file.exists (home <- file.path (local, name, version))) dir.create (home, recursive = TRUE);
      for (file in c (...)) file.copy (file, home);
      'ok';
    }, error = function (error) {
      unlink (home, recursive = TRUE);
      stop (error);
    })) (file.path (base, 'raven.R'));
  };
  on.install;
}, binder, TRUE);

test <- function (execution) function (binder, name, version, info) {
  info ("Testing", paste (name, version, sep = ':'));
  define ('execution', function () execution, b = binder);
  inject (function (on.test = 'not applicable') on.test, binder, TRUE);
};

raven <- function (...,
                   base = getwd (), parent = binder (), restore.libPaths = FALSE, log = 'info',
                   local = path.expand (file.path ('~', '.raven', 'repository')))
  binder (parent = parent, callback = function (binder) tryCatch ({
    save <- .libPaths ();

    define ('debug', function () function (...) {}, b = binder);
    define ('info', function () function (...) {}, b = binder);
    define ('warn', function () function (...) {}, b = binder);
    define ('error', function () function (...) cat ('[ERROR]', ..., '\n'), b = binder);
    if (log != 'error') {
      define ('warn', function () function (...) cat ('[WARN]', ..., '\n'), b = binder);
      if (log != 'warn') {
        define ('info', function () function (...) cat ('[INFO]', ..., '\n'), b = binder);
        if (log != 'info') {
          define ('debug', function () function (...) cat ('[DEBUG]', ..., '\n'), b = binder);
          if (log != 'debug') define (log, function () function (...) cat (paste ('[', toupper (log), ']'), ..., '\n'), b = binder);
        }
      }
    }

    define ('base', function () base, eager, binder);
    define ('binder', function () binder, eager, binder);
    define ('log', function () log, eager, binder);
    define ('local', function () local, eager, binder);
    goals <- c (...);
    inject (function (execution = parent.frame (1))
                        if (length (goals) < 1)
                          goals <<- c (inspect (), fetch (FALSE), import (execution), test (new.env (parent = execution)), install ()),
                      binder);
    define ('goals', function () goals, b = binder);

    for (goal in goals) inject (goal, b = binder);

    'ok';
  }, finally = if (restore.libPaths) .libPaths (save)));
