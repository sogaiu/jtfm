# this is meant to be executed from the repository root directory

# this path is relative to test dir
(import ../jtfm)

(when (= :jeep (dyn :test/runner)) (print))

(jtfm/main)

