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
    (break [:no-tests nil]))
  #
  (def {:report report} opts)
  (default report o/report)
  # print out results
  (report test-results test-out test-err)
  #
  (when (not= 0 ecode)
    (break [:ecode test-results]))
  #
  (os/rm test-path)
  [:no-fails test-results])

(defn c/make-run-report
  [src-paths opts]
  (def b @{:in "make-run-report" :args {:src-paths src-paths :opts opts}})
  #
  (def excludes (get opts :excludes))
  (def p-paths @[])
  (def f-paths @[])
  (def test-results @[])
  # generate tests, run tests, and report
  (each path src-paths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (l/note :i path)
      (def single-result (c/mrr-single path opts))
      (put b :locals @{:single-result single-result :path path})
      (def [desc tr] single-result)
      (array/push test-results [path tr])
      (case desc
        :no-tests
        (l/noten :i " - no tests found")
        #
        :no-fails
        (let [n-tests (get tr :num-tests)
              ratio (o/color-ratio n-tests n-tests)]
          (l/notenf :i " - [%s]" ratio)
          (array/push p-paths path))
        #
        :ecode
        (let [n-fails (length (get tr :fails))
              n-tests (get tr :num-tests)
              ratio (o/color-ratio n-fails n-tests)]
          (l/notenf :i "[%s]" ratio)
          (array/push f-paths path))
        #
        (e/emf b "unexpected result %p for: %s" desc path))))
  #
  (l/notenf :i (o/separator "="))
  (def n-f-paths (length f-paths))
  (def n-p-paths (length p-paths))
  #
  (if (empty? f-paths)
    (l/notenf :i "All tests successful in %d file(s)."
              n-p-paths)
    (l/notenf :i "Test failures in %d of %d file(s)."
              n-f-paths (+ n-f-paths n-p-paths)))
  #
  [(if (zero? n-f-paths) 0 1)
   test-results])

########################################################################

(defn c/mru-single
  [input &opt opts]
  (def b @{:in "make-run-update" :args {:input input :opts opts}})
  # try to make and run tests, then collect output
  (def [ecode test-path test-results _ _] (c/make-and-run input opts))
  (when (= :no-tests ecode)
    (break [:no-tests nil test-results]))
  # successful run means no tests to update
  (when (zero? ecode)
    (os/rm test-path)
    (break [:no-updates nil test-results]))
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
    [:single-update lines test-results]
    [:multi-update lines test-results]))

(defn c/make-run-update
  [src-paths opts]
  (def b @{:in "make-run-update" :args {:src-paths src-paths :opts opts}})
  #
  (def excludes (get opts :excludes))
  (def upd-paths @[])
  (def test-results @[])
  # generate tests, run tests, and update
  (each path src-paths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (def single-result (c/mru-single path opts))
      (put b :locals @{:path path :single-result single-result})
      (def [desc data tr] single-result)
      (array/push test-results [path tr])
      (case desc
        :no-tests
        (l/noten :i "no tests found")
        #
        :no-updates
        (l/noten :i "no tests needed updating")
        #
        :multi-update
        (let [cs-lines (string/join (map |(string $) data) ", ")]
          (array/push upd-paths path)
          (l/notenf :i "Test(s) updated in: %s on lines: %s"
                    path cs-lines))
        #
        :single-update
        (let [first-line (get data 0)]
          (array/push upd-paths path)
          (l/notenf :i "Test updated in: %s on line: %d"
                    path first-line)
          (break))
        #
        (e/emf b "unexpected result %n for: %s" desc path))))
  #
  (l/notenf :i "Test(s) updated in %d file(s)." (length upd-paths))
  #
  [0 test-results])

