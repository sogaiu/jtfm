(import ./jipper :as j)
(import ./verify :as v)

# at its simplest, a test is expressed like:
#
# (comment
#
#   (+ 1 1)
#   # =>
#   2
#
#   )
#
# i.e. inside a comment form, a single test consists of:
#
# * a test expression        - `(+ 1 1)`
# * a test indicator         - `# =>`
# * an expected expression   - `2`
#
# there can be one or more tests within a comment form.

# ti == test indicator, which can look like any of:
#
# # =>
# # before =>
# # => after
# # before => after
#
# further constraint that neither `before` nor `after` should contain
# a hash character (#)

(defn find-test-indicator
  [zloc]
  (var label-left nil)
  (var label-right nil)
  [(j/right-until zloc
                  |(match (j/node $)
                     [:comment _ content]
                     (if-let [[l r]
                              (peg/match ~(sequence "#"
                                                    (capture (to "=>"))
                                                    "=>"
                                                    (capture (thru -1)))
                                         content)
                              no-hash-left (nil? (string/find "#" l))
                              no-hash-right (nil? (string/find "#" r))]
                       (do
                         (set label-left (string/trim l))
                         (set label-right (string/trim r))
                         true)
                       false)))
   label-left
   label-right])

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(+ 1 1)" eol
            "# =>"    eol
            "2"))

  (let [[zloc l r]
        (find-test-indicator (-> (j/par src)
                                 j/zip-down))]
    (and zloc
         (empty? l)
         (empty? r)))
  # =>
  true

  (def src
    (string "(+ 1 1)"     eol
            "# before =>" eol
            "2"))

  (let [[zloc l r]
        (find-test-indicator (-> (j/par src)
                                 j/zip-down))]
    (and zloc
         (= "before" l)
         (empty? r)))
  # =>
  true

  (def src
    (string "(+ 1 1)"    eol
            "# => after" eol
            "2"))

  (let [[zloc l r]
        (find-test-indicator (-> (j/par src)
                                 j/zip-down))]
    (and zloc
         (empty? l)
         (= "after" r)))
  # =>
  true

  )

(defn find-test-expr
  [ti-zloc]
  # check for appropriate conditions "before"
  (def before-zlocs @[])
  (var curr-zloc ti-zloc)
  (var found-before nil)
  # collect zlocs to the left of the test indicator up through the
  # first non-whitespace/comment one.  if there is a
  # non-whitespace/comment one, that is the test expression.
  (while curr-zloc
    (set curr-zloc (j/left curr-zloc))
    (when (nil? curr-zloc)
      (break))
    #
    (match (j/node curr-zloc)
      [:comment]
      (array/push before-zlocs curr-zloc)
      #
      [:whitespace]
      (array/push before-zlocs curr-zloc)
      #
      (do
        (set found-before true)
        (array/push before-zlocs curr-zloc)
        (break))))
  #
  (cond
    (nil? curr-zloc)
    :no-test-expression
    # if all collected zlocs (except the last one) are whitespace,
    # then the test expression has been located
    (and found-before
         (->> (slice before-zlocs 0 -2)
              (filter |(not (match (j/node $)
                              [:whitespace]
                              true)))
              length
              zero?))
    curr-zloc
    #
    :unexpected-result))

(comment

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # =>"           eol
            "  @{:a 2}"        eol
            eol
            "  )"))

  (def [ti-zloc _ _]
    (find-test-indicator (-> (j/par src)
                             j/zip-down
                             j/down)))

  (j/node ti-zloc)
  # =>
  [:comment @{:bc 3 :bl 6 :ec 7 :el 6} "# =>"]

  (def test-expr-zloc (find-test-expr ti-zloc))

  (j/node test-expr-zloc)
  # =>
  [:tuple @{:bc 3 :bl 5 :ec 17 :el 5}
   [:symbol @{:bc 4 :bl 5 :ec 7 :el 5} "put"]
   [:whitespace @{:bc 7 :bl 5 :ec 8 :el 5} " "]
   [:table @{:bc 8 :bl 5 :ec 11 :el 5}]
   [:whitespace @{:bc 11 :bl 5 :ec 12 :el 5} " "]
   [:keyword @{:bc 12 :bl 5 :ec 14 :el 5} ":a"]
   [:whitespace @{:bc 14 :bl 5 :ec 15 :el 5} " "]
   [:number @{:bc 15 :bl 5 :ec 16 :el 5} "2"]]

  (-> (j/left test-expr-zloc)
      j/node)
  # =>
  [:whitespace @{:bc 1 :bl 5 :ec 3 :el 5} "  "]

  )

(defn find-expected-expr
  [ti-zloc]
  (def after-zlocs @[])
  (var curr-zloc ti-zloc)
  (var found-comment nil)
  (var found-after nil)
  # collect zlocs to the right of the test indicator up through the
  # first non-whitespace/comment one.  if there is a
  # non-whitespace/comment one, that is the expression used to compute
  # the expected value.
  (while curr-zloc
    (set curr-zloc (j/right curr-zloc))
    (when (nil? curr-zloc)
      (break))
    #
    (match (j/node curr-zloc)
      [:comment]
      (do
        (set found-comment true)
        (break))
      #
      [:whitespace]
      (array/push after-zlocs curr-zloc)
      #
      (do
        (set found-after true)
        (array/push after-zlocs curr-zloc)
        (break))))
  #
  (cond
    (or (nil? curr-zloc)
        found-comment)
    :no-expected-expression
    # if there was a non-whitespace/comment zloc and the first zloc
    # "captured" represents eol (i.e. the first zloc to the right of
    # the test indicator), then there might be a an "expected
    # expression" that follows...
    (and found-after
         (match (j/node (first after-zlocs))
           [:whitespace _ "\n"]
           true
           [:whitespace _ "\r\n"]
           true))
    # starting on the line after the eol zloc, keep collected zlocs up
    # to (but not including) another eol zloc.  the first
    # non-whitespace zloc of the kept zlocs represents the "expected
    # expression".
    (if-let [from-next-line (drop 1 after-zlocs)
             before-eol-zloc (take-until |(match (j/node $)
                                            [:whitespace _ "\n"]
                                            true
                                            [:whitespace _ "\r\n"]
                                            true)
                                         from-next-line)
             target (->> before-eol-zloc
                         (filter |(match (j/node $)
                                    [:whitespace]
                                    false
                                    #
                                    true))
                         first)]
      target
      :no-expected-expression)
    #
    :unexpected-result))

(comment

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # =>"           eol
            "  @{:a 1"         eol
            "    :b 2}"        eol
            eol
            "  )"))

  (def [ti-zloc _ _]
    (find-test-indicator (-> (j/par src)
                             j/zip-down
                             j/down)))

  (j/node ti-zloc)
  # =>
  [:comment @{:bc 3 :bl 6 :ec 7 :el 6} "# =>"]

  (def expected-expr-zloc (find-expected-expr ti-zloc))

  (j/node expected-expr-zloc)
  # =>
  [:table @{:bc 3 :bl 7 :ec 10 :el 8}
   [:keyword @{:bc 5 :bl 7 :ec 7 :el 7} ":a"]
   [:whitespace @{:bc 7 :bl 7 :ec 8 :el 7} " "]
   [:number @{:bc 8 :bl 7 :ec 9 :el 7} "1"]
   [:whitespace @{:bc 9 :bl 7 :ec 1 :el 8} "\n"]
   [:whitespace @{:bc 1 :bl 8 :ec 5 :el 8} "    "]
   [:keyword @{:bc 5 :bl 8 :ec 7 :el 8} ":b"]
   [:whitespace @{:bc 7 :bl 8 :ec 8 :el 8} " "]
   [:number @{:bc 8 :bl 8 :ec 9 :el 8} "2"]]

  (-> (j/left expected-expr-zloc)
      j/node)
  # =>
  [:whitespace @{:bc 1 :bl 7 :ec 3 :el 7} "  "]

  (def src
    (string "(comment"                eol
            eol
            "  (butlast @[:a :b :c])" eol
            "  # => @[:a :b]"         eol
            eol
            "  (butlast [:a])"        eol
            "  # => []"               eol
            eol
            ")"))

  (def [ti-zloc _ _]
    (find-test-indicator (-> (j/par src)
                             j/zip-down
                             j/down)))

  (j/node ti-zloc)
  # =>
  [:comment @{:bc 3 :bl 4 :ec 16 :el 4} "# => @[:a :b]"]

  (find-expected-expr ti-zloc)
  # =>
  :no-expected-expression

  )

(defn make-label
  [left right]
  (string ""
          (when (not (empty? left))
            (string " " left))
          (when (or (not (empty? left))
                    (not (empty? right)))
            (string " =>"))
          (when (not (empty? right))
            (string " " right))))

(comment

  (make-label "hi" "there")
  # =>
  " hi => there"

  (make-label "hi" "")
  # =>
  " hi =>"

  (make-label "" "there")
  # =>
  " => there"

  (make-label "" "")
  # =>
  ""

  )

(defn find-test-exprs
  [ti-zloc]
  # look for a test expression
  (def test-expr-zloc (find-test-expr ti-zloc))
  (case test-expr-zloc
    :no-test-expression
    (break [nil nil])
    #
    :unexpected-result
    (errorf "unexpected result from `find-test-expr`: %p"
            test-expr-zloc))
  # look for an expected value expression
  (def expected-expr-zloc (find-expected-expr ti-zloc))
  (case expected-expr-zloc
    :no-expected-expression
    (break [test-expr-zloc nil])
    #
    :unexpected-result
    (errorf "unexpected result from `find-expected-expr`: %p"
            expected-expr-zloc))
  #
  [test-expr-zloc expected-expr-zloc])

(defn wrap-as-test-call
  [start-zloc end-zloc test-label]
  # XXX: hack - not sure if robust enough
  (def eol-str (if (= :windows (os/which)) "\r\n" "\n"))
  (-> (j/wrap start-zloc [:tuple @{}] end-zloc)
      # newline important for preserving long strings
      (j/insert-child [:whitespace @{} eol-str])
      # name of test macro
      (j/insert-child [:symbol @{} "_verify/is"])
      # for column zero convention, insert leading whitespace
      # before the beginning of the tuple (_verify/is ...)
      (j/insert-left [:whitespace @{} "  "])
      # add location info argument
      (j/append-child [:whitespace @{} " "])
      (j/append-child [:string @{} test-label])))

(defn rewrite-comment-zloc
  [comment-zloc]
  # move into comment block
  (var curr-zloc (j/down comment-zloc))
  (var found-test nil)
  # process comment block content
  (while (not (j/end? curr-zloc))
    (def [ti-zloc label-left label-right] (find-test-indicator curr-zloc))
    (when (not ti-zloc)
      (break))
    #
    (def [test-expr-zloc expected-expr-zloc] (find-test-exprs ti-zloc))
    (set curr-zloc
         (if (or (nil? test-expr-zloc)
                 (nil? expected-expr-zloc))
           (j/right curr-zloc) # next
           # found a complete test, work on rewriting
           (let [left-of-te-zloc (j/left test-expr-zloc)
                 start-zloc (match (j/node left-of-te-zloc)
                              [:whitespace]
                              left-of-te-zloc
                              #
                              test-expr-zloc)
                 end-zloc expected-expr-zloc
                 # XXX: use `attrs` here?
                 ti-line-no ((get (j/node ti-zloc) 1) :bl)
                 test-label (string `"`
                                    `line-` ti-line-no
                                    (make-label label-left label-right)
                                    `"`)]
             (set found-test true)
             (wrap-as-test-call start-zloc end-zloc test-label)))))
  # navigate back out to top of block
  (when found-test
    # morph comment block into plain tuple -- to be unwrapped later
    (-> curr-zloc
        j/up
        j/down
        (j/replace [:whitespace @{} " "])
        # begin hack to prevent trailing whitespace once unwrapping occurs
        j/rightmost
        (j/insert-right [:keyword @{} ":smile"])
        # end of hack
        j/up)))

(comment

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # left =>"      eol
            "  @{:a 2}"        eol
            eol
            "  (+ 1 1)"        eol
            "  # => right"     eol
            "  2"              eol
            eol
            "  )"))

  (-> (j/par src)
      j/zip-down
      rewrite-comment-zloc
      j/root
      j/gen)
  # =>
  (string "( "                          eol
          eol
          "  (def a 1)"                 eol
          eol
          "  (_verify/is"               eol
          "  (put @{} :a 2)"            eol
          "  # left =>"                 eol
          `  @{:a 2} "line-6 left =>")` eol
          eol
          "  (_verify/is"               eol
          "  (+ 1 1)"                   eol
          "  # => right"                eol
          `  2 "line-10 => right")`     eol
          eol
          "  :smile)")

  )

(defn rewrite-comment-block
  [comment-src]
  (-> (j/par comment-src)
      j/zip-down
      rewrite-comment-zloc
      j/root
      j/gen))

(comment

  (def src
    (string "(comment"          eol
            eol
            "  (def a 1)"       eol
            eol
            "  (put @{} :a 2)"  eol
            "  # =>"            eol
            "  @{:a 2}"         eol
            eol
            "  (+ 1 1)"         eol
            "  # left => right" eol
            "  2"               eol
            eol
            "  )"))

  (rewrite-comment-block src)
  # =>
  (string "( "                           eol
          eol
          "  (def a 1)"                  eol
          eol
          "  (_verify/is"                eol
          "  (put @{} :a 2)"             eol
          "  # =>"                       eol
          `  @{:a 2} "line-6")`          eol
          eol
          "  (_verify/is"                eol
          "  (+ 1 1)"                    eol
          "  # left => right"            eol
          `  2 "line-10 left => right")` eol
          eol
          "  :smile)")

  )

(defn rewrite
  [src]
  (var changed nil)
  # XXX: hack - not sure if robust enough
  (def eol-str (if (= :windows (os/which)) "\r\n" "\n"))
  (var curr-zloc
    (-> (j/par src)
        j/zip-down
        # XXX: leading newline is a hack to prevent very first thing
        #      from being a comment block
        (j/insert-left [:whitespace @{} eol-str])
        # XXX: once the newline is inserted, need to move to it
        j/left))
  #
  (while (not (j/end? curr-zloc))
    # try to find a top-level comment block
    (if-let [comment-zloc
             (j/right-until curr-zloc
                            |(match (j/node $)
                               [:tuple _ [:symbol _ "comment"]]
                               true))]
      # may be rewrite the located top-level comment block
      (set curr-zloc
           (if-let [rewritten-zloc
                    (rewrite-comment-zloc comment-zloc)]
             (do
               (set changed true)
               (j/unwrap rewritten-zloc))
             comment-zloc))
      (break)))
  #
  (when changed
    (-> curr-zloc
        j/root
        j/gen)))

(comment

  (def src
    (string `(require "json")` eol
            eol
            "(defn my-fn"      eol
            "  [x]"            eol
            "  (+ x 1))"       eol
            eol
            "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # =>"           eol
            "  @{:a 2}"        eol
            eol
            "  (my-fn 1)"      eol
            "  # =>"           eol
            "  2"              eol
            eol
            "  )"              eol
            eol
            "(defn your-fn"    eol
            "  [y]"            eol
            "  (* y y))"       eol
            eol
            "(comment"         eol
            eol
            "  (your-fn 3)"    eol
            "  # =>"           eol
            "  9"              eol
            eol
            "  (def b 1)"      eol
            eol
            "  (+ b 1)"        eol
            "  # =>"           eol
            "  2"              eol
            eol
            "  (def c 2)"      eol
            eol
            "  )"              eol
            ))

  (rewrite src)
  # =>
  (string eol
          `(require "json")`     eol
          eol
          "(defn my-fn"          eol
          "  [x]"                eol
          "  (+ x 1))"           eol
          eol
          " "                    eol
          eol
          "  (def a 1)"          eol
          eol
          "  (_verify/is"        eol
          "  (put @{} :a 2)"     eol
          "  # =>"               eol
          `  @{:a 2} "line-12")` eol
          eol
          "  (_verify/is"        eol
          "  (my-fn 1)"          eol
          "  # =>"               eol
          `  2 "line-16")`       eol
          eol
          "  :smile"             eol
          eol
          "(defn your-fn"        eol
          "  [y]"                eol
          "  (* y y))"           eol
          eol
          " "                    eol
          eol
          "  (_verify/is"        eol
          "  (your-fn 3)"        eol
          "  # =>"               eol
          `  9 "line-28")`       eol
          eol
          "  (def b 1)"          eol
          eol
          "  (_verify/is"        eol
          "  (+ b 1)"            eol
          "  # =>"               eol
          `  2 "line-34")`       eol
          eol
          "  (def c 2)"          eol
          eol
          "  :smile"             eol)

  )

(comment

  # https://github.com/sogaiu/judge-gen/issues/1
  (def src
    (string "(comment"        eol
            eol
            "  (-> ``"        eol
            "      123456789" eol
            "      ``"        eol
            "      length)"   eol
            "  # =>"          eol
            "  9"             eol
            eol
            "  (->"           eol
            "    ``"          eol
            "    123456789"   eol
            "    ``"          eol
            "    length)"     eol
            "  # =>"          eol
            "  9"             eol
            eol
            "  )"))

  (rewrite src)
  # =>
  (string eol
          " "               eol
          eol
          "  (_verify/is"   eol
          "  (-> ``"        eol
          "      123456789" eol
          "      ``"        eol
          "      length)"   eol
          "  # =>"          eol
          `  9 "line-7")`   eol
          eol
          "  (_verify/is"   eol
          "  (->"           eol
          "    ``"          eol
          "    123456789"   eol
          "    ``"          eol
          "    length)"     eol
          "  # =>"          eol
          `  9 "line-15")`  eol
          eol
          "  :smile")

  )

(defn rewrite-as-test-file
  [src]
  (when (not (empty? src))
    (when-let [rewritten (rewrite src)]
      # XXX: hack - not sure if robust enough
      (def eol-str (if (= :windows (os/which)) "\r\n" "\n"))
      (string v/as-string
              eol-str
              "(_verify/start-tests)"
              eol-str
              rewritten
              eol-str
              "(_verify/end-tests)"
              eol-str
              "(_verify/report)"
              eol-str))))

