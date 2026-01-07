(import ../src/main)

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (main/main ""
                 (string "{:raw true\n"
                         " :no-color true\n"
                         " :no-exit true}")
                 "data/all-tests-pass"))
    (parse buf))
  # =>
  (parse (slurp "data/all-tests-pass.txt"))

  )

