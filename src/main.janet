(import ./args :as a)
(import ./rewrite :as r)
(import ./search :as s)
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
  (when (not test-src)
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
  [test-filepath &opt opts]
  (default opts {})
  (def {:no-color no-color} opts)
  (def ose-flags (if no-color :pe :p))
  (try
    (with [of (file/temp)]
      (with [ef (file/temp)]
        (let [# prevents any contained `main` functions from executing
              cmd
              ["janet" "-e" (string "(dofile `" test-filepath "`)")]
              # when trying to update, use NO_COLOR
              ecode
              (os/execute cmd ose-flags
                          (merge {:out of :err ef}
                                 {"NO_COLOR" (when no-color "1")}))]
          (when (not (zero? ecode))
            (eprintf "non-zero exit code: %p" ecode))
          #
          (file/flush of)
          (file/flush ef)
          (file/seek of :set 0)
          (file/seek ef :set 0)
          #
          [ecode
           (file/read of :all)
           (file/read ef :all)])))
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

# sample output:

``
--(1)--

failed:
line-4

form:
(+ 1 2)

expected:
2

actual:
3

--(2)--

failed:
line-8

form:
(- 1 1)

expected:
1

actual:
0

------------------------------------------------------------
0 of 2 passed
------------------------------------------------------------
``

# XXX: work on "output as data" later
(defn parse-out
  [out]
  # see verify.janet
  (def dashes (string/repeat "-" 60))
  # remove from dashes onwards and trim the left end of the output
  (def dashes-idx (string/find dashes out))
  (def truncated (string/triml (string/slice out 0 dashes-idx)))
  (def m (peg/match ~(some (sequence "--(" :d+ ")--"
                                     (thru "\n")
                                     (thru "\nfailed:")
                                     (thru "\nline-")
                                     (number :d+)
                                     (thru "\n")
                                     (thru "\nform:")
                                     (thru "\n")
                                     (thru "\nexpected:")
                                     (thru "\nactual:")
                                     (thru "\n")
                                     (capture (to "\n"))
                                     (thru "\n")
                                     (thru "\n")
                                     (choice (look 0 "--(")
                                             -1)))
                    truncated))
  (if m
    (table ;m)
    nil))

(defn make-and-run
  [filepath &opt opts]
  (default opts @{})
  # create test source
  (def result (make-tests filepath opts))
  (when (not result)
    (eprintf "failed to create test file for: %p" filepath)
    (break [nil nil nil nil]))
  #
  (when (= :no-tests result)
    (break [:no-tests nil nil nil]))
  #
  (def test-filepath result)
  (def [ecode out err] (run-tests test-filepath opts))
  # run tests and collect output
  [ecode test-filepath out err])

(defn make-run-report
  [filepath &opt opts]
  # try to make and run tests, then collect output
  (def [ecode test-filepath out err] (make-and-run filepath opts))
  (when (or (nil? ecode) (= :no-tests ecode))
    (break ecode))
  # print out results
  (report out err)
  # finish off
  (when (zero? ecode)
    (os/rm test-filepath)
    true))

(defn make-run-update
  [filepath &opt opts]
  # try to make and run tests, then collect output
  (def [ecode test-filepath out err] (make-and-run filepath opts))
  (when (or (nil? ecode) (= :no-tests ecode))
    (break ecode))
  # successful run means no tests to update
  (when (zero? ecode)
    (os/rm test-filepath)
    (break true))
  #
  (def parsed (parse-out out))
  (when (not parsed)
    (eprintf "failed to parse test output: %s" out)
    (break nil))
  #
  (def lines-tbl
    (if (get opts :update-first)
      (let [key-0 (get (sort (keys parsed)) 0)]
        @{key-0 (get parsed key-0)})
      parsed))
  (def ret (r/patch-file filepath lines-tbl))
  (when (not ret)
    (eprintf "failed to patch file: %s" filepath)
    (break nil))
  #
  (os/rm test-filepath)
  (break (if (get opts :update-first)
           :stop
           :continue)))

########################################################################

(defn main
  [& args]
  (def opts (a/parse-args (drop 1 args)))
  #
  (when (get opts :show-help)
    (print usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (print version)
    (os/exit 0))
  #
  (def update? (or (get opts :update) (get opts :update-first)))
  #
  (def includes (get opts :includes))
  (def excludes (get opts :excludes))
  #
  (def src-filepaths
    (s/collect-paths includes |(or (string/has-suffix? ".janet" $)
                                   (s/has-janet-shebang? $))))
  # generate tests, run tests, and update / report
  (each path src-filepaths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (print path)
      (def result
        (if update?
          (make-run-update path (merge opts {:no-color true}))
          (make-run-report path opts)))
      (cond
        (= :stop result)
        (do
          (printf "Test updated in: %s" path)
          (os/exit 0))
        #
        (= :continue result)
        (printf "Test(s) updated in: %s" path)
        #
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
  #
  (when (not update?)
    (printf "All tests completed successfully in %d file(s)."
            (length src-filepaths))))

