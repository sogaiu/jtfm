(import ./args :prefix "")
(import ./commands :prefix "")
(import ./errors :prefix "")
(import ./log :prefix "")
(import ./search :prefix "")

###########################################################################

(def version "DEVEL")

(def usage
  ``
  Usage: jtfm [<file-or-dir>...]
         jtfm [-h|--help] [-v|--version]

  Create and run comment tests...just the facts, ma'am.

  Parameters:

    <file-or-dir>          path to file or directory

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Configuration:

    .jtfm.jdn              configuration file

  Examples:

    Create and run tests in `src/` directory:

    $ jtfm src

    `jtfm` can be used via `jpm`, `jeep`, etc. with
    some one-time setup.  Create a suitable `.jtfm.jdn`
    file in a project's root directory and a runner
    file in a project's `test/` subdirectory (see below
    for further details).

    Run via `jeep test`:

    $ jeep test

    Run via `jpm test`:

    $ jpm test

    Run using the configuration file via direct
    invocation:

    $ jtfm

  Example `.jtfm.jdn` content:

    {# describes what to test - file and dir paths
     :includes ["src" "bin/my-script"]
     # describes what to skip - file paths only
     :excludes ["src/sample.janet"]}

  Example runner file `test/trigger-jtfm.janet`:

    (import ../jtfm)

    (jtfm/main)
  ``)

########################################################################

(defn main
  [& args]
  (def opts (a/parse-args (drop 1 args)))
  #
  (when (get opts :show-help)
    (l/logf usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (l/logf version)
    (os/exit 0))
  #
  (def src-paths
    (s/collect-paths (get opts :includes)
                     |(or (string/has-suffix? ".janet" $)
                          (s/has-janet-shebang? $))))
  #
  (try
    (if (or (get opts :update) (get opts :update-first))
      (c/make-run-update src-paths opts)
      (c/make-run-report src-paths opts))
    ([e f]
      (if (dictionary? e)
        (e/show e)
        (debug/stacktrace f e "internal "))
      (os/exit 1))))

