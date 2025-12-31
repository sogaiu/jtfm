# XXX: try to put in file?  had trouble originally when working on
#      judge-gen.  may be will have more luck?
(def as-string
  ``
  # influenced by janet's tools/helper.janet

  (var _verify/start-time 0)
  (var _verify/end-time 0)
  (var _verify/test-results @[])

  (defmacro _verify/is
    [t-form e-form &opt name]
    (default name
      (string "test-" (inc (length _verify/test-results))))
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
    # find test failures
    (def fails (filter |(not (get $ :passed)) _verify/test-results))
    # prepare test results
    (def test-results @{:num-tests (length _verify/test-results)})
    (def safe-fails
      (map (fn [f]
             (def t-value (get f :test-value))
             (def [tr ts] (protect (string/format "%j" t-value)))
             (when (not tr)
               (-> f
                   (put :test-value (string/format "%m" t-value))
                   (put :test-unreadable true)))
             (def e-value (get f :expected-value))
             (def [er es] (protect (string/format "%j" e-value)))
             (when (not er)
               (-> f
                   (put :expected-value (string/format "%m" e-value))
                   (put :expected-unreadable true)))
             #
             f)
           fails))
    (put test-results :fails safe-fails)
    # report test results
    (printf "%j" test-results)
    # abort if there were any failures
    (when (not (empty? fails))
      (os/exit 1)))
  ``)

(def color-table
  {:black 30
   :blue 34
   :cyan 36
   :green 32
   :magenta 35
   :red 31
   :white 37
   :yellow 33})

(defn print-color
  [msg color]
  (def color-num (get color-table color))
  (assertf color-num "unknown color: %n" color)
  (def real-msg
    (if (os/getenv "NO_COLOR")
      msg
      (string "\e[" color-num "m" msg "\e[0m")))
  (prin real-msg))

(comment

  (def [ok? result] (protect (print-color "hey" :chartreuse)))
  # =>
  [false "unknown color: :chartreuse"]

  )

(defn dashes
  [&opt n]
  (default n 60)
  (string/repeat "-" n))

(defn print-dashes
  [&opt n]
  (print (dashes n)))

(defn print-form
  [form &opt color]
  (def buf @"")
  (with-dyns [:out buf]
    (printf "%m" form))
  (def msg (string/trimr buf))
  (print ":")
  (if color
    (print-color msg color)
    (prin msg))
  (print))

(defn report
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
    (print-color i :cyan)
    (print ")--")
    (print)
    #
    (print-color "failed:" :yellow)
    (print)
    (print-color test-name :red)
    (print)
    #
    (print)
    (print-color "form" :yellow)
    (print-form test-form)
    #
    (print)
    (print-color "expected" :yellow)
    (print-form expected-value)
    #
    (print)
    (print-color "actual" :yellow)
    (print-form test-value :blue))
  (when (zero? (length fails))
    (print)
    (print "No tests failed."))
  # summarize totals
  (print)
  (print-dashes)
  (when (= 0 total-tests)
    (print "No tests found, so no judgements made.")
    (break true))
  (if (not= total-passed total-tests)
    (print-color total-passed :red)
    (print-color total-passed :green))
  (prin " of ")
  (print-color total-tests :green)
  (print " passed")
  (print-dashes)
  # extra newlines from original report function handling out
  (print)
  (print))

(defn report-stderr
  [err]
  (when (and err (pos? (length err)))
    (print "------")
    (print "stderr")
    (print "------")
    (print err)
    (print)))

