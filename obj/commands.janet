(import ./errors :prefix "")
(import ./log :prefix "")
(import ./output :prefix "")
(import ./rewrite :prefix "")
(import ./tests :prefix "")

(defn c/make-and-run
  [input &opt opts]
  (def b @{:in "make-and-run" :args {:input input :opts opts}})
  #
  (default opts @{})
  # create test source
  (def result (t/make-tests input opts))
  (when (not result)
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

########################################################################

(defn c/mrr-single
  [input &opt opts]
  # try to make and run tests, then collect output
  (def [ecode test-path test-results test-out test-err]
    (c/make-and-run input opts))
  (when (= :no-tests ecode)
    (break :no-tests))
  #
  (def {:report report} opts)
  (default report o/report)
  # print out results
  (report test-results test-out test-err)
  #
  (when (not= 0 ecode)
    (break ecode))
  #
  (os/rm test-path)
  true)

(defn c/make-run-report
  [src-paths opts]
  (def b @{:in "make-run-report" :args {:src-paths src-paths :opts opts}})
  #
  (def excludes (get opts :excludes))
  (def td-paths @[])
  # generate tests, run tests, and report
  (each path src-paths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (l/logf path)
      (def result (c/mrr-single path opts))
      (put b :locals @{:result result :path path})
      (cond
        (= true result)
        (array/push td-paths path)
        #
        (= :no-tests result)
        # XXX: the 2 newlines here are cosmetic
        (l/elogf "* no tests detected for: %s\n\n" path)
        #
        (int? result)
        (e/emf b "exit code %d while testing: %s" result path)
        #
        (e/emf b "unexpected result %p for: %s" result path))))
  #
  (l/logf "All tests completed successfully in %d file(s)."
          (length td-paths)))

########################################################################

(defn c/mru-single
  [input &opt opts]
  (def b @{:in "make-run-update" :args {:input input :opts opts}})
  # try to make and run tests, then collect output
  (def [ecode test-path test-results _ _] (c/make-and-run input opts))
  (when (= :no-tests ecode)
    (break :no-tests))
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
  (def lines (map |(get $ 0) update-info))
  #
  (if (get opts :update-first)
    [:stop lines]
    [:continue lines]))

(defn c/make-run-update
  [src-paths opts]
  (def b @{:in "make-run-update" :args {:src-paths src-paths :opts opts}})
  #
  (def excludes (get opts :excludes))
  (def upd-paths @[])
  # generate tests, run tests, and update
  (each path src-paths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (def result (c/mru-single path opts))
      (put b :locals @{:path path :result result})
      (cond
        (= true result)
        true
        #
        (= :no-tests result)
        # XXX: the 2 newlines here are cosmetic
        (l/elogf "* no tests detected for: %s\n\n" path)
        #
        (and (tuple? result) (= 2 (length result)))
        (let [[action lines] result]
          (cond
            (= :continue action)
            (let [cs-lines (string/join (map |(string $) lines) ", ")]
              (array/push upd-paths path)
              (l/logf "Test(s) updated in: %s on lines: %s"
                      path cs-lines))
            #
            (= :stop action)
            (let [first-line (get lines 0)]
              (array/push upd-paths path)
              (l/logf "Test updated in: %s on line: %d" path first-line)
              (break))
            #
            (e/emf b "unknown action: %n" action)))
        #
        (e/emf b "unexpected result %p for: %s" result path))))
  #
  (l/logf "Test(s) updated in %d file(s)." (length upd-paths)))

