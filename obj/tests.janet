(import ./log :prefix "")
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
    (l/elogf "test file already exists for: %s" filepath)
    (break nil))
  #
  (spit test-filepath test-src)
  #
  test-filepath)

(defn t/run-tests
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

