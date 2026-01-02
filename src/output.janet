(import ./log :as l)

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
  (l/lof real-msg))

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
  (l/log (dashes n)))

(defn print-form
  [form &opt color]
  (def buf @"")
  (with-dyns [:out buf]
    (printf "%m" form))
  (def msg (string/trimr buf))
  (l/log ":")
  (if color
    (print-color msg color)
    (l/lof msg))
  (l/log))

(defn legacy-report
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
    (print-color i :cyan)
    (l/log ")--")
    (l/log)
    #
    (print-color "failed:" :yellow)
    (l/log)
    (print-color (string/format "line-%d" line-no) :red)
    (l/log)
    #
    (l/log)
    (print-color "form" :yellow)
    (print-form test-form)
    #
    (l/log)
    (print-color "expected" :yellow)
    (print-form expected-value)
    #
    (l/log)
    (print-color "actual" :yellow)
    (print-form test-value :blue))
  (when (zero? (length fails))
    (l/log)
    (l/log "No tests failed."))
  # summarize totals
  (l/log)
  (print-dashes)
  (when (= 0 total-tests)
    (l/log "No tests found, so no judgements made.")
    (break true))
  (if (not= total-passed total-tests)
    (print-color total-passed :red)
    (print-color total-passed :green))
  (l/lof " of ")
  (print-color total-tests :green)
  (l/log " passed")
  (print-dashes)
  # extra newlines from original report function handling out
  (l/log)
  (l/log))

(defn report-stderr
  [err]
  (when (and err (pos? (length err)))
    (l/log "------")
    (l/log "stderr")
    (l/log "------")
    (l/log err)
    (l/log)))

(defn report
  [test-results err]
  (legacy-report test-results)
  (when (and err (pos? (length err)))
    (report-stderr err)
    (l/log))
  (when (and (zero? (get test-results :num-tests))
             (empty? err))
    (l/log "no test output...possibly no tests")
    (l/log)))

