(import ./args :as a)
(import ./errors :as e)
(import ./log :as l)
(import ./rewrite :as r)
(import ./search :as s)
(import ./tests :as t)
(import ./output :as o)

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

(defn make-and-run
  [input &opt opts]
  (def b @{:in "make-and-run" :args {:input input :opts opts}})
  #
  (default opts @{})
  # create test source
  (def result (t/make-tests input opts))
  (when (not result)
    (e/emf b "failed to create test file for: %n" input))
  #
  (when (= :no-tests result)
    (break [:no-tests nil nil nil]))
  #
  (def test-path result)
  # run tests and collect output
  (def [ecode out err] (t/run-tests test-path))
  #
  (when (empty? out)
    (e/emf (merge b {:locals {:ecode ecode :out out :err err}})
           "out should be non-empty - review verify.janet"))
  #
  (def [test-results test-out] (t/parse-output out))
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
  (def fmt-str
    (string/format "unreadable value in:\n%s"
                   (if (get opts :no-color) "%m" "%M")))
  (when test-unreadable?
    (e/emf b fmt-str test-unreadable?))
  #
  (when expected-unreadable?
    (e/emf b fmt-str expected-unreadable?))
  #
  [ecode test-path test-results test-out err])

(defn make-run-report
  [input &opt opts]
  # try to make and run tests, then collect output
  (def [ecode test-path test-results test-out test-err]
    (make-and-run input opts))
  (when (or (nil? ecode) (= :no-tests ecode))
    (break ecode))
  #
  (def {:report report} opts)
  (default report o/report)
  # print out results
  (report test-results test-out test-err)
  # finish off
  (when (zero? ecode)
    (os/rm test-path)
    true))

(defn make-run-update
  [input &opt opts]
  (def b @{:in "make-run-update" :args {:input input :opts opts}})
  # try to make and run tests, then collect output
  (def [ecode test-path test-results _ _] (make-and-run input opts))
  (when (or (nil? ecode) (= :no-tests ecode))
    (break ecode))
  # successful run means no tests to update
  (when (zero? ecode)
    (os/rm test-path)
    (break true))
  #
  (def fails (get test-results :fails))
  (def update-info
    (seq [f :in (if (get opts :update-first)
                  @[(get fails 0)]
                  fails)
          :let [{:line-no line-no :test-value test-value} f
                tv-str (string/format "%j" test-value)]]
      [line-no tv-str]))
  (def ret (r/patch input update-info))
  (when (not ret)
    (e/emf (merge b {:locals {:fails fails :update-info update-info}})
           "failed to patch: %n" input))
  #
  (os/rm test-path)
  #
  (if (get opts :update-first)
    :stop
    :continue))

########################################################################

(defn main
  [& args]
  (def b @{:in "main" :args {:args args}})
  #
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
  (def update? (or (get opts :update) (get opts :update-first)))
  #
  (def includes (get opts :includes))
  (def excludes (get opts :excludes))
  #
  (def src-paths
    (s/collect-paths includes |(or (string/has-suffix? ".janet" $)
                                   (s/has-janet-shebang? $))))
  #
  (put b :locals @{:opts opts :update? update?
                   :includes includes :excludes excludes
                   :src-paths src-paths})
  # generate tests, run tests, and update / report
  (each path src-paths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (put-in b [:locals :path] path)
      (try
        (do
          (l/logf path)
          (def result (if update?
                        (make-run-update path opts)
                        (make-run-report path opts)))
          (put-in b [:locals :result] result)
          (cond
            (= :stop result)
            (do
              (l/logf "Test updated in: %s" path)
              (os/exit 0))
            #
            (= :continue result)
            (l/logf "Test(s) updated in: %s" path)
            #
            (= :no-tests result)
            # XXX: the 2 newlines here are cosmetic
            (l/elogf "* no tests detected for: %s\n\n" path)
            #
            (nil? result)
            (e/emf b "failure in: %s" path)
            #
            (true? result)
            true
            #
            (e/emf b "unexpected result %p for: %s" result path)))
        ([e]
          (e/show e)
          (os/exit 1)))))
  #
  (when (not update?)
    (l/logf "All tests completed successfully in %d file(s)."
            (length src-paths))))

