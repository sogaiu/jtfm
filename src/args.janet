(import ./errors :as e)

(defn parse-args
  [args]
  (def b {:in "parse-args" :args {:args args}})
  #
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (def conf-file ".jtfm.jdn")
  #
  (when (or (= head "-h") (= head "--help")
            # might have been invoked with no paths in repository root
            (and (not head)
                 (not= :file (os/stat conf-file :mode))))
    (break @{:show-help true}))
  #
  (when (or (= head "-v") (= head "--version")
            # might have been invoked with no paths in repository root
            (and (not head)
                 (not= :file (os/stat conf-file :mode))))
    (break @{:show-version true}))
  #
  (def opts
    (if head
      (if-not (and (string/has-prefix? "{" head)
                   (string/has-suffix? "}" head))
        @{}
        (let [parsed
              (try (parse (string "@" head))
                ([e] (e/emf (merge b {:e-via-try e})
                            "failed to parse options: %n" head)))]
          (when (not (and parsed (table? parsed)))
            (e/emf b "expected table but found: %s" (type parsed)))
          #
          (array/remove the-args 0)
          parsed))
      @{}))
  #
  (def [includes excludes]
    (cond
      # paths on command line take precedence over conf file
      (not (empty? the-args))
      [the-args @[]]
      # conf file
      (= :file (os/stat conf-file :mode))
      (let [src (try (slurp conf-file)
                  ([e] (e/emf (merge b {:e-via-try e})
                              "failed to slurp: %s" conf-file)))
            cnf (try (parse src)
                  ([e] (e/emf (merge b {:e-via-try e})
                              "failed to parse: %s" conf-file)))]
        (when (not cnf)
          (e/emf b "failed to load: %s" conf-file))
        #
        (when (not (dictionary? cnf))
          (e/emf b "expected dictionary in conf, got: %s" (type cnf)))
        #
        [(array ;(get cnf :includes @[]))
         (array ;(get cnf :excludes @[]))])
      #
      (e/emf b "unexpected result parsing args: %n" args)))
  # XXX: struct has precedence over env var...is that desirable?
  (when (and (not (false? (get opts :no-color)))
             (os/getenv "NO_COLOR"))
    (put opts :no-color true))
  #
  (defn merge-indexed
    [left right]
    (default left [])
    (default right [])
    (distinct [;left ;right]))
  #
  (merge opts
         {:includes (merge-indexed includes (get opts :includes))
          :excludes (merge-indexed excludes (get opts :excludes))}))

(comment

  (def old-val (os/getenv "NO_COLOR"))

  (os/setenv "NO_COLOR" nil)

  (parse-args ["src/main.janet"])
  # =>
  @{:excludes @[]
    :includes @["src/main.janet"]}

  (parse-args ["-h"])
  # =>
  @{:show-help true}

  (parse-args ["{:overwrite true}" "src/main.janet"])
  # =>
  @{:excludes @[]
    :includes @["src/main.janet"]
    :overwrite true}

  (parse-args [`{:excludes ["src/args.janet"]}` "src/main.janet"])
  # =>
  @{:excludes @["src/args.janet"]
    :includes @["src/main.janet"]}

  (os/setenv "NO_COLOR" old-val)

  )

