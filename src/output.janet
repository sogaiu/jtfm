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

(defn color-msg
  [msg color]
  (def color-num (get color-table color))
  (assertf color-num "unknown color: %n" color)
  (def real-msg
    (if (os/getenv "NO_COLOR")
      msg
      (string "\e[" color-num "m" msg "\e[0m")))
  #
  real-msg)

(defn prin-color
  [msg color]
  (l/note :o (color-msg msg color)))

(comment

  (def [ok? result] (protect (prin-color "hey" :chartreuse)))
  # =>
  [false "unknown color: :chartreuse"]

  )

(defn dashes
  [&opt n]
  (default n 60)
  (string/repeat "-" n))

(defn prin-dashes
  [&opt n]
  (l/note :o (dashes n)))

(defn prin-form
  [form &opt color]
  (def buf @"")
  (with-dyns [:out buf]
    (printf "%m" form))
  (def msg (string/trimr buf))
  (def m-buf
    (buffer ":\n"
            (if color (color-msg msg color) msg)))
  (l/note :o m-buf))

(defn prin-summary
  [total-tests num-fails]
  (def total-passed (- total-tests num-fails))
  (l/note :o "[")
  (if (not= total-passed total-tests)
    (prin-color total-passed :red)
    (prin-color total-passed :green))
  (l/note :o "/")
  (prin-color total-tests :green)
  (l/noten :o "]"))

(defn report-fails
  [{:num-tests total-tests :fails fails}]
  (var i 0)
  (each f fails
    (def {:test-value test-value
          :expected-value expected-value
          :line-no line-no
          :test-form test-form} f)
    (++ i)
    #
    (l/noten :o)
    (l/note :o "[")
    (prin-color i :cyan)
    (l/note :o "]")
    (l/noten :o)
    #
    (l/noten :o)
    (prin-color "failed:" :yellow)
    (l/noten :o)
    (prin-color (string/format "line %d" line-no) :red)
    (l/noten :o)
    #
    (l/noten :o)
    (prin-color "form" :yellow)
    (prin-form test-form)
    (l/noten :o)
    #
    (l/noten :o)
    (prin-color "expected" :yellow)
    (prin-form expected-value)
    (l/noten :o)
    #
    (l/noten :o)
    (prin-color "actual" :yellow)
    (prin-form test-value :blue)
    (l/noten :o)))

(defn report-std
  [content title]
  (when (and content (pos? (length content)))
    (def separator (string/repeat "-" (length title)))
    (l/noten :o separator)
    (l/noten :o title)
    (l/noten :o separator)
    (l/noten :o content)))

(defn report
  [test-results out err]
  #
  (def failures? (not (empty? (get test-results :fails))))
  #
  (when failures?
    (l/noten :o)
    (prin-dashes))
  #
  (report-fails test-results)
  #
  (when (and out (pos? (length out)))
    (l/noten :o)
    (report-std out "stdout"))
  #
  (when (and err (pos? (length err)))
    (l/noten :o)
    (report-std err "stderr"))
  #
  (when (and (zero? (get test-results :num-tests))
             (empty? out)
             (empty? err))
    (l/noten :o)
    (l/noten :o "no test output...possibly no tests"))
  #
  (when failures?
    (prin-dashes)
    (l/noten :o)))

