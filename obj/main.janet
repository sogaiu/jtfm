(import ./args :prefix "")
(import ./rewrite :prefix "")
(import ./search :prefix "")
(import ./tests :prefix "")
(import ./verify :prefix "")

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

(defn make-and-run
  [filepath &opt opts]
  (default opts @{})
  # create test source
  (def result (t/make-tests filepath opts))
  (when (not result)
    (eprintf "failed to create test file for: %s" filepath)
    (break [nil nil nil nil]))
  #
  (when (= :no-tests result)
    (break [:no-tests nil nil nil]))
  #
  (def test-filepath result)
  # run tests and collect output
  (def [ecode out err] (t/run-tests test-filepath opts))
  #
  (when (empty? out)
    (eprintf "expected non-empty output")
    (eprintf "possible problem in verify.janet"))
  #
  (def test-results (parse out))
  (def fails (get test-results :fails))
  (var test-unreadable? nil)
  (var expected-unreadable? nil)
  (each f fails
    (when (get f :test-unreadable)
      (set test-unreadable? f)
      (break))
      #
    (when (get f :expected-unreadable)
      (set expected-unreadable? f)
      (break)))
  #
  (def fmt-str (if (get opts :no-color) "%m" "%M"))
  (when test-unreadable?
    (eprint "unreadable value in test-value")
    (eprintf fmt-str test-unreadable?)
    (break [nil nil nil nil]))
  #
  (when expected-unreadable?
    (eprint "unreadable value in expected-value")
    (eprintf fmt-str expected-unreadable?)
    (break [nil nil nil nil]))
  #
  [ecode test-filepath test-results err])

(defn report
  [test-results err]
  (v/report test-results)
  (when (and err (pos? (length err)))
    (v/report-stderr err)
    (print))
  (when (and (zero? (get test-results :num-tests))
             (empty? err))
    (print "no test output...possibly no tests")
    (print)))

(defn make-run-report
  [filepath &opt opts]
  # try to make and run tests, then collect output
  (def [ecode test-filepath test-results err] (make-and-run filepath opts))
  (when (or (nil? ecode) (= :no-tests ecode))
    (break ecode))
  # print out results
  (report test-results err)
  # finish off
  (when (zero? ecode)
    (os/rm test-filepath)
    true))

(defn make-run-update
  [filepath &opt opts]
  # try to make and run tests, then collect output
  (def [ecode test-filepath test-results err] (make-and-run filepath opts))
  (when (or (nil? ecode) (= :no-tests ecode))
    (break ecode))
  # successful run means no tests to update
  (when (zero? ecode)
    (os/rm test-filepath)
    (break true))
  #
  (def fails (get test-results :fails))
  (def update-info
    (seq [f :in (if (get opts :update-first)
                  @[(get fails 0)]
                  fails)
          :let [{:name name :test-value test-value} f
                line-no (-> name
                            # assumes `name` is like `line-11`
                            (string/slice (length "line-"))
                            scan-number)
                tv-str (string/format "%j" test-value)]]
      [line-no tv-str]))
  (def ret (r/patch-file filepath update-info))
  (when (not ret)
    (eprintf "failed to patch file: %s" filepath)
    (break nil))
  #
  (os/rm test-filepath)
  #
  (if (get opts :update-first)
    :stop
    :continue))

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
        (eprintf "* no tests detected for: %s\n\n" path)
        #
        (nil? result)
        (do
          (eprintf "failure in: %s" path)
          (os/exit 1))
        #
        (true? result)
        true
        #
        (do
          (eprintf "Unexpected result %p for: %s" result path)
          (os/exit 1)))))
  #
  (when (not update?)
    (printf "All tests completed successfully in %d file(s)."
            (length src-filepaths))))

