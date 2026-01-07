(import ../src/main)

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (main/main "" 
                 (string "{:raw true\n"
                         " :no-color true\n"
                         " :no-exit true\n"
                         " :overwrite true}")
                 "data/some-tests-fail"))
    (parse buf))
  # =>
  (parse (slurp "data/some-tests-fail.txt"))

  )

