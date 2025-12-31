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

(defn t/parse-out
  [out]
  (def [headers-blob body] (string/split "\n\n" out 0 2))
  (def headers
    (peg/match ~{:eol (choice "\r\n" "\n")
                 :line (sequence (capture (to (choice :eol -1)))
                                 (choice :eol -1))
                 :main (some :line)}
               headers-blob))
  (def meta @{})
  (each h headers
    (def [name value] (string/split ": " h))
    (put meta (string/ascii-lower name) value))
  (def boundary (get meta "boundary"))
  (assertf boundary "expected non-empty boundary")
  (def total-fails (scan-number (get meta "fails")))
  (assertf (number? total-fails) "expected number but found: %n"
           (get meta "fails"))
  (def total-tests (scan-number (get meta "tests")))
  (assertf (number? total-tests) "expected number but found: %n"
           (get meta "tests"))
  (def raw-fails
    (if (= 0 total-fails) # no "boundary" marker
      # if delimiter not found, string/split returns single element
      # and that is not desired, so handle separately
      @[]
      (string/split boundary body 0 total-fails)))
  (def fails @[])
  (var unreadable nil)
  (each rf raw-fails
    (def [ok? f] (protect (parse rf)))
    (when (not ok?)
      (set unreadable rf)
      (break))
    #
    (array/push fails f))
  #
  (if unreadable
    unreadable
    @{:total-tests total-tests
      :total-fails total-fails
      :fails fails}))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def output
    (string
      "Tests: 3"                                                      eol
      "Fails: 2"                                                      eol
      "Boundary: ########"                                            eol
      eol
      "{ :expected-form @[:xa]"                                       eol
      "  :expected-status true"                                       eol
      "  :expected-value @[:xa]"                                      eol
      `  :name "line-8"`                                              eol
      "  :passed false"                                               eol
      "  :test-form (array/concat @[] :a)"                            eol
      "  :test-status true"                                           eol
      "  :test-value @[:a]}"                                          eol
      "########"                                                      eol
      "{ :expected-form 0"                                            eol
      "  :expected-status true"                                       eol
      "  :expected-value 0"                                           eol
      `  :name "line-12"`                                             eol
      "  :test-form (- 1 :a)"                                         eol
      "  :test-status false"                                          eol
      `  :test-value "could not find method :- for 1 or :r- for :a"}` eol
      "########"                                                      eol))

  (t/parse-out output)
  # =>
  '@{:fails
     @[{:expected-form @[:xa]
        :expected-status true
        :expected-value @[:xa]
        :name "line-8"
        :passed false
        :test-form (array/concat @[] :a)
        :test-status true
        :test-value @[:a]}
       {:expected-form 0
        :expected-status true
        :expected-value 0
        :name "line-12"
        :test-form (- 1 :a)
        :test-status false
        :test-value "could not find method :- for 1 or :r- for :a"}]
    :total-fails 2
    :total-tests 3}

  (def erroring-output
    (string
      "Tests: 1"                          eol
      "Fails: 1"                          eol
      "Boundary: ########"                eol
      eol
      "{ :expected-form 0"                eol
      "  :expected-status true"           eol
      "  :expected-value 0"               eol
      `  :name "line-4"`                  eol
      "  :passed false"                   eol
      "  :test-form printf"               eol
      "  :test-status true"               eol
      "  :test-value <cfunction printf>}" eol
      "########"                          eol))

  (protect (t/parse-out erroring-output))
  # =>
  [true
   (string
     "{ :expected-form 0\n"
     "  :expected-status true\n"
     "  :expected-value 0\n"
     "  :name \"line-4\"\n"
     "  :passed false\n"
     "  :test-form printf\n"
     "  :test-status true\n"
     "  :test-value <cfunction printf>}\n"
     "########\n")]

  )

