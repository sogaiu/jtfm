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
         # XXX: consider base64-encoding or other for test-value
         #      and expected-value as this might help parsing the
         #      string passed back to the calling process
         (array/push _verify/test-results
                     {:test-form ',t-form
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
    (var total-tests 0)
    (var total-passed 0)
    # analyze results
    (def fails @[])
    (each tr _verify/test-results
      (++ total-tests)
      (def {:passed test-passed} tr)
      (if test-passed
        (++ total-passed)
        (array/push fails tr)))
    # report any failures
    # XXX: boundary marker should not be too predicatable
    (def boundary (string/format "# ! * %f * ! #" (os/clock)))
    (print "Tests: " total-tests)
    (print "Fails: " (length fails))
    (print "Boundary: " boundary)
    (print)
    (each f fails
      (printf "%m" f)
      (print boundary))
    (when (not= total-passed total-tests)
      (os/exit 1)))
  ``)

(defn print-color
  [msg color]
  # XXX: what if color doesn't match...
  (let [color-num (match color
                    :black 30
                    :blue 34
                    :cyan 36
                    :green 32
                    :magenta 35
                    :red 31
                    :white 37
                    :yellow 33)]
    (def real-msg
      (if (os/getenv "NO_COLOR")
        msg
        (string "\e[" color-num "m" msg "\e[0m")))
    (prin real-msg)))

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
  [{:total-tests total-tests :fails fails}]
  (def total-passed (- total-tests (length fails)))
  (var i 0)
  (each fail fails
    (def {:test-value test-value
          :expected-value expected-value
          :name test-name
          :passed test-passed
          :test-form test-form} fail)
    (++ i)
    (print)
    (prin "--(")
    (print-color i :cyan)
    (print ")--")
    (print)

    (print-color "failed:" :yellow)
    (print)
    (print-color test-name :red)
    (print)

    (print)
    (print-color "form" :yellow)
    (print-form test-form)

    (print)
    (print-color "expected" :yellow)
    (print-form expected-value)

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

