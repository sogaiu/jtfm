(import ./args :as a)
(import ./search :as s)
(import ./rewrite :as r)
(import ./utils :as u)

###########################################################################

(def version "DEVEL")

(def usage
  ``
  Usage: jtfm [<file-or-dir>...]
         jtfm [-h|--help]

  Create and run comment tests...just the facts, ma'am.

  Parameters:

    <file-or-dir>          path to file or directory

  Options:

    -h, --help             show this output

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

(def test-file-ext ".jtfm")

(defn make-tests
  [filepath &opt opts]
  (def src (slurp filepath))
  (def test-src (r/rewrite-as-test-file src))
  (unless test-src
    (break :no-tests))
  #
  (def [fdir fname] (u/parse-path filepath))
  (def test-filepath (string fdir "_" fname test-file-ext))
  (when (and (not (get opts :overwrite))
             (os/stat test-filepath :mode))
    (eprintf "test file already exists for: %p" filepath)
    (break nil))
  #
  (spit test-filepath test-src)
  #
  test-filepath)

(defn run-tests
  [test-filepath]
  (try
    (with [of (file/temp)]
      (with [ef (file/temp)]
        (let [cmd
              # prevents any contained `main` functions from executing
              ["janet" "-e" (string "(dofile `" test-filepath "`)")]
              ecode (os/execute cmd :p {:out of :err ef})]
          (when (not (zero? ecode))
            (eprintf "non-zero exit code: %p" ecode))
          #
          (file/flush of)
          (file/flush ef)
          (file/seek of :set 0)
          (file/seek ef :set 0)
          #
          [(file/read of :all)
           (file/read ef :all)
           ecode])))
    ([e]
      (eprintf "problem executing tests: %p" e)
      [nil nil nil])))

(defn report
  [out err]
  (when (and out (pos? (length out)))
    (print out)
    (print))
  (when (and err (pos? (length err)))
    (print "------")
    (print "stderr")
    (print "------")
    (print err)
    (print))
  # XXX: kind of awkward
  (when (and (empty? out) (empty? err))
    (print "no test output...possibly no tests")
    (print)))

(defn make-run-report
  [filepath &opt opts]
  (default opts @{})
  # create test source
  (def result (make-tests filepath opts))
  (unless result
    (eprintf "failed to create test file for: %p" filepath)
    (break nil))
  #
  (when (= :no-tests result)
    (break :no-tests))
  #
  (def test-filepath result)
  # run tests and collect output
  (def [out err ecode] (run-tests test-filepath))
  # print out results
  (report out err)
  # finish off
  (when (zero? ecode)
    (os/rm test-filepath)
    true))

########################################################################

(defn main
  [& args]
  (def opts (a/parse-args (drop 1 args)))
  #
  (when (get opts :show-help)
    (print usage)
    (os/exit 0))
  #
  (def includes (get opts :includes))
  (def excludes (get opts :excludes))
  #
  (def src-filepaths
    (s/collect-paths includes |(or (string/has-suffix? ".janet" $)
                                   (s/has-janet-shebang? $))))
  # generate tests, run tests, and report
  (each path src-filepaths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (print path)
      (def result (make-run-report path opts))
      (cond
        (= :no-tests result)
        # XXX: the 2 newlines here are cosmetic
        (eprintf "* no tests detected for: %p\n\n" path)
        #
        (nil? result)
        (do
          (eprintf "failure in: %p" path)
          (os/exit 1))
        #
        (true? result)
        true
        #
        (do
          (eprintf "Unexpected result %p for: %p" result path)
          (os/exit 1)))))
  (printf "All tests completed successfully in %d file(s)."
          (length src-filepaths)))

