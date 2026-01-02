(import ./log :prefix "")

(def o/color-table
  {:black 30
   :blue 34
   :cyan 36
   :green 32
   :magenta 35
   :red 31
   :white 37
   :yellow 33})

(defn o/print-color
  [msg color]
  (def color-num (get o/color-table color))
  (assertf color-num "unknown color: %n" color)
  (def real-msg
    (if (os/getenv "NO_COLOR")
      msg
      (string "\e[" color-num "m" msg "\e[0m")))
  (l/lof real-msg))

(comment

  (def [ok? result] (protect (o/print-color "hey" :chartreuse)))
  # =>
  [false "unknown color: :chartreuse"]

  )

(defn o/dashes
  [&opt n]
  (default n 60)
  (string/repeat "-" n))

(defn o/print-dashes
  [&opt n]
  (l/log (o/dashes n)))

(defn o/print-form
  [form &opt color]
  (def buf @"")
  (with-dyns [:out buf]
    (printf "%m" form))
  (def msg (string/trimr buf))
  (l/log ":")
  (if color
    (o/print-color msg color)
    (l/lof msg))
  (l/log))

(defn o/legacy-report
  [{:num-tests total-tests :fails fails}]
  (def total-passed (- total-tests (length fails)))
  (var i 0)
  (each f fails
    (def {:test-value test-value
          :expected-value expected-value
          :name test-name
          :line-no line-no
          :passed test-passed
          :test-form test-form} f)
    (++ i)
    (l/log)
    (l/lof "--(")
    (o/print-color i :cyan)
    (l/log ")--")
    (l/log)
    #
    (o/print-color "failed:" :yellow)
    (l/log)
    (o/print-color (string/format "line-%d" line-no) :red)
    (l/log)
    #
    (l/log)
    (o/print-color "form" :yellow)
    (o/print-form test-form)
    #
    (l/log)
    (o/print-color "expected" :yellow)
    (o/print-form expected-value)
    #
    (l/log)
    (o/print-color "actual" :yellow)
    (o/print-form test-value :blue))
  (when (zero? (length fails))
    (l/log)
    (l/log "No tests failed."))
  # summarize totals
  (l/log)
  (o/print-dashes)
  (when (= 0 total-tests)
    (l/log "No tests found, so no judgements made.")
    (break true))
  (if (not= total-passed total-tests)
    (o/print-color total-passed :red)
    (o/print-color total-passed :green))
  (l/lof " of ")
  (o/print-color total-tests :green)
  (l/log " passed")
  (o/print-dashes)
  # extra newlines from original report function handling out
  (l/log)
  (l/log))

(defn o/report-std
  [content title]
  (when (and content (pos? (length content)))
    (def separator (string/repeat "-" (length title)))
    (l/log separator)
    (l/log title)
    (l/log separator)
    (l/log content)))

(defn o/report
  [test-results out err]
  (o/legacy-report test-results)
  (when (and out (pos? (length out)))
    (o/report-std out "stdout")
    (l/log))
  (when (and err (pos? (length err)))
    (o/report-std err "stderr")
    (l/log))
  (when (and (zero? (get test-results :num-tests))
             (empty? out)
             (empty? err))
    (l/log "no test output...possibly no tests")
    (l/log)))

