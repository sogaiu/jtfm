(import ./log :as l)
(import ./rewrite :as r)
(import ./utils :as u)

(def test-file-ext ".jtfm")

(defn make-tests
  [filepath &opt opts]
  (def src (slurp filepath))
  (def test-src (r/rewrite-as-test-file src))
  (when (not test-src)
    (break :no-tests))
  #
  (def [fdir fname] (u/parse-path filepath))
  (def test-filepath (string fdir "_" fname test-file-ext))
  (when (and (not (get opts :overwrite))
             (os/stat test-filepath :mode))
    (l/elogf "test file already exists for: %s" filepath)
    (break nil))
  #
  (spit test-filepath test-src)
  #
  test-filepath)

(defn run-tests
  [test-filepath]
  (try
    (with [of (file/temp)]
      (with [ef (file/temp)]
        (let [# prevents any contained `main` functions from executing
              cmd
              ["janet" "-e" (string "(dofile `" test-filepath "`)")]
              ecode
              (os/execute cmd :p {:out of :err ef})]
          (when (not (zero? ecode))
            (l/elogf "non-zero exit code: %d" ecode))
          #
          (file/flush of)
          (file/flush ef)
          (file/seek of :set 0)
          (file/seek ef :set 0)
          #
          [ecode
           (file/read of :all)
           (file/read ef :all)])))
    ([e]
      (l/elogf "problem executing tests: %p" e)
      [nil nil nil])))

(defn parse-output
  [out]
  # see verify.janet
  (def boundary (buffer/new-filled 72 (chr "#")))
  (def b-idx (last (string/find-all boundary out)))
  (assertf b-idx "failed to find boundary in output: %n" out)
  (def [test-out results] (string/split boundary out b-idx))
  #
  [(parse results) test-out])

(comment

  (def data
    {:test-form '(+ 1 1)
     :test-status true
     :test-value 2
     :expected-form 3
     :expected-status true
     :expected-value 3
     :line-no 4
     :passed true
     :name ""})

  (def separator (buffer/new-filled 72 (chr "#")))

  (def out
    (string
      "hello this is a line\n"
      "and so is this\n"
      separator "\n"
      (string/format "%j" data)))

  (parse-output out)
  # =>
  [{:expected-form 3
    :expected-status true
    :expected-value 3
    :line-no 4
    :name ""
    :passed true
    :test-form '(+ 1 1)
    :test-status true
    :test-value 2}
   "hello this is a line\nand so is this\n"]

  )

