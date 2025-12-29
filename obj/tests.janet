(import ./rewrite :prefix "")
(import ./utils :prefix "")

(def t/test-file-ext ".jtfm")

(defn t/make-tests
  [filepath &opt opts]
  (def src (slurp filepath))
  (def test-src (r/rewrite-as-test-file src))
  (when (not test-src)
    (break :no-tests))
  #
  (def [fdir fname] (u/parse-path filepath))
  (def test-filepath (string fdir "_" fname t/test-file-ext))
  (when (and (not (get opts :overwrite))
             (os/stat test-filepath :mode))
    (eprintf "test file already exists for: %p" filepath)
    (break nil))
  #
  (spit test-filepath test-src)
  #
  test-filepath)

(defn t/run-tests
  [test-filepath &opt opts]
  (default opts {})
  (def {:no-color no-color} opts)
  (def ose-flags (if no-color :pe :p))
  (try
    (with [of (file/temp)]
      (with [ef (file/temp)]
        (let [# prevents any contained `main` functions from executing
              cmd
              ["janet" "-e" (string "(dofile `" test-filepath "`)")]
              # when trying to update, use NO_COLOR
              ecode
              (os/execute cmd ose-flags
                          (merge {:out of :err ef}
                                 {"NO_COLOR" (when no-color "1")}))]
          (when (not (zero? ecode))
            (eprintf "non-zero exit code: %p" ecode))
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
      (eprintf "problem executing tests: %p" e)
      [nil nil nil])))

# XXX: work on "output as data" later
(defn t/parse-out
  [out]
  # see verify.janet
  (def dashes (string/repeat "-" 60))
  # remove from dashes onwards and trim the left end of the output
  (def dashes-idx (string/find dashes out))
  (def truncated (string/triml (string/slice out 0 dashes-idx)))
  (def m (peg/match ~(some (sequence "--(" :d+ ")--"
                                     (thru "\n")
                                     (thru "\nfailed:")
                                     (thru "\nline-")
                                     (number :d+)
                                     (thru "\n")
                                     (thru "\nform:")
                                     (thru "\n")
                                     (thru "\nexpected:")
                                     (thru "\nactual:")
                                     (thru "\n")
                                     (capture (to "\n"))
                                     (thru "\n")
                                     (thru "\n")
                                     (choice (look 0 "--(")
                                             -1)))
                    truncated))
  (if m
    (table ;m)
    nil))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def output
    (string
      "--(1)--"   eol
      eol
      "failed:"   eol
      "line-4"    eol
      eol
      "form:"     eol
      "(+ 1 2)"   eol
      eol
      "expected:" eol
      "2"         eol
      eol
      "actual:"   eol
      "3"         eol
      eol
      "--(2)--"   eol
      eol
      "failed:"   eol
      "line-8"    eol
      eol
      "form:"     eol
      "(- 1 1)"   eol
      eol
      "expected:" eol
      "1"         eol
      eol
      "actual:"   eol
      "0"         eol
      eol
      "------------------------------------------------------------" eol
      "0 of 2 passed" eol
      "------------------------------------------------------------"))

  (t/parse-out output)
  # =>
  @{4 "3"
    8 "0"}

  )

