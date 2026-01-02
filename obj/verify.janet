# XXX: try to put in file?  had trouble originally when working on
#      judge-gen.  may be will have more luck?
(def v/as-string
  ``
  # influenced by janet's tools/helper.janet

  (var _verify/start-time 0)
  (var _verify/end-time 0)
  (var _verify/test-results @[])

  (defmacro _verify/is
    [t-form e-form line-no name]
    (with-syms [$ts $tr
                $es $er]
      ~(do
         (def [,$ts ,$tr] (protect (eval ',t-form)))
         (def [,$es ,$er] (protect (eval ',e-form)))
         (array/push _verify/test-results
                     @{:test-form ',t-form
                       :test-status ,$ts
                       :test-value ,$tr
                       #
                       :expected-form ',e-form
                       :expected-status ,$es
                       :expected-value ,$er
                       #
                       :line-no ,line-no
                       :name ,name
                       :passed (if (and ,$ts ,$es)
                                 (deep= ,$tr ,$er)
                                 nil)})
         ,name)))

  (defn _verify/start-tests
    []
    (set _verify/start-time (os/clock))
    (set _verify/test-results @[]))

  (defn _verify/end-tests
    []
    (set _verify/end-time (os/clock)))

  (defn _verify/report
    []
    # find and massage failures
    (def fails
      (keep (fn [r]
              (when (not (get r :passed))
                (def t-value (get r :test-value))
                (def [tr ts] (protect (string/format "%j" t-value)))
                (when (not tr)
                  (-> r
                      (put :test-value (string/format "%m" t-value))
                      (put :test-unreadable true)))
                (def e-value (get r :expected-value))
                (def [er es] (protect (string/format "%j" e-value)))
                (when (not er)
                  (-> r
                      (put :expected-value (string/format "%m" e-value))
                      (put :expected-unreadable true)))
                #
                r))
            _verify/test-results))
    # prepare test results
    (def test-results
      @{:num-tests (length _verify/test-results)
        :fails fails})
    # report test results
    (printf "%j" test-results)
    # signal if there were any failures
    (when (not (empty? fails))
      (os/exit 1)))
  ``)

(def v/color-table
  {:black 30
   :blue 34
   :cyan 36
   :green 32
   :magenta 35
   :red 31
   :white 37
   :yellow 33})

(defn v/print-color
  [msg color]
  (def color-num (get v/color-table color))
  (assertf color-num "unknown color: %n" color)
  (def real-msg
    (if (os/getenv "NO_COLOR")
      msg
      (string "\e[" color-num "m" msg "\e[0m")))
  (prin real-msg))

(comment

  (def [ok? result] (protect (v/print-color "hey" :chartreuse)))
  # =>
  [false "unknown color: :chartreuse"]

  )

(defn v/dashes
  [&opt n]
  (default n 60)
  (string/repeat "-" n))

(defn v/print-dashes
  [&opt n]
  (print (v/dashes n)))

(defn v/print-form
  [form &opt color]
  (def buf @"")
  (with-dyns [:out buf]
    (printf "%m" form))
  (def msg (string/trimr buf))
  (print ":")
  (if color
    (v/print-color msg color)
    (prin msg))
  (print))

(defn v/report
  [{:num-tests total-tests :fails fails}]
  (def total-passed (- total-tests (length fails)))
  (var i 0)
  (each f fails
    (def {:test-value test-value
          :expected-value expected-value
          :name test-name
          :passed test-passed
          :test-form test-form} f)
    (++ i)
    (print)
    (prin "--(")
    (v/print-color i :cyan)
    (print ")--")
    (print)
    #
    (v/print-color "failed:" :yellow)
    (print)
    (v/print-color test-name :red)
    (print)
    #
    (print)
    (v/print-color "form" :yellow)
    (v/print-form test-form)
    #
    (print)
    (v/print-color "expected" :yellow)
    (v/print-form expected-value)
    #
    (print)
    (v/print-color "actual" :yellow)
    (v/print-form test-value :blue))
  (when (zero? (length fails))
    (print)
    (print "No tests failed."))
  # summarize totals
  (print)
  (v/print-dashes)
  (when (= 0 total-tests)
    (print "No tests found, so no judgements made.")
    (break true))
  (if (not= total-passed total-tests)
    (v/print-color total-passed :red)
    (v/print-color total-passed :green))
  (prin " of ")
  (v/print-color total-tests :green)
  (print " passed")
  (v/print-dashes)
  # extra newlines from original report function handling out
  (print)
  (print))

(defn v/report-stderr
  [err]
  (when (and err (pos? (length err)))
    (print "------")
    (print "stderr")
    (print "------")
    (print err)
    (print)))

