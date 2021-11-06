(ns calva.fmt.editor
  (:require [calva.fmt.util :as util]))


(defn raplacement-edits-for-diffing-lines
  "Returns a vector of replacement edits to apply to `old-text` to get `new-text`.

   Edits will be in the form
  `{:edit \"replace\"
    :start {:line line :character 0}
    :end   {:line line :character 5}
    :text \"hello\"}`,

   NB: The two versions need to have the same amount of lines or an assertion will be thrown"
  [old-text new-text]
  (loop [text-index-old 0
         text-index-new 0
         row 0
         edits []]
    (let [next-newline-old (.indexOf old-text "\n" text-index-old)
          next-newline-new (.indexOf new-text "\n" text-index-new)
          _ (assert (= (neg? next-newline-old)
                       (neg? next-newline-new))
                    "the two strings doesn't have the same number of rows!")
          ;; are we on the last row, ie next-newline idx is -1?
          last-row? (and (neg? next-newline-old)
                         (neg? next-newline-new))

          ;; when we are in last row of the both texts, take the
          ;; rest of the text in the row - (subs s from),
          ;; otherwise                     (subs s from to-exclusive)
          old-text-row (if last-row? (subs old-text text-index-old)
                           ;; check if char before new-line is an \r,
                           (let [row-end-old (if (= "\r" (subs old-text (dec next-newline-old) next-newline-old))
                                               ;; then ignore it further on
                                               (dec next-newline-old)
                                               next-newline-old)]
                             (subs old-text text-index-old row-end-old)))
          new-text-row (if last-row? (subs new-text text-index-new)
                           ;; check if char before new-line is an \r,
                           (let [row-end-new (if (= "\r" (subs new-text (dec next-newline-new) next-newline-new))
                                               ;;then ignore it further on
                                               (dec next-newline-new)
                                               next-newline-new)]
                             (subs new-text text-index-new row-end-new)))
          edits (if (= old-text-row new-text-row)
                  edits
                  (conj edits {:edit "replace"
                               :start {:line row :character 0}
                               :end {:line row :character (count new-text-row)}
                               :text new-text-row}))]
      (if last-row?
        edits
        (recur (inc next-newline-old) (inc next-newline-new) (inc row) edits)))))

(comment
  ;; testing \r\n line breaks
  (= (raplacement-edits-for-diffing-lines "foo\r\nfooo\r\nbar\r\nbar"  "foo\r\nbar\r\nbaz\r\nbar")
     [{:edit "replace", :start {:line 1, :character 0}, :end {:line 1, :character 3}, :text "bar"}
      {:edit "replace", :start {:line 2, :character 0}, :end {:line 2, :character 3}, :text "baz"}])

  ;; testing \n line breaks
  (= (raplacement-edits-for-diffing-lines "foo\nfooo\nbar\nbar"  "foo\nbar\nbaz\nbar")
     [{:edit "replace", :start {:line 1, :character 0}, :end {:line 1, :character 3}, :text "bar"}
      {:edit "replace", :start {:line 2, :character 0}, :end {:line 2, :character 3}, :text "baz"}])

  ;; testing text ending with \n
  (= (raplacement-edits-for-diffing-lines "foo\nfooo\nbar\nbar\n"  "foo\nbar\nbaz\nbar\n")
     [{:edit "replace", :start {:line 1, :character 0}, :end {:line 1, :character 3}, :text "bar"}
      {:edit "replace", :start {:line 2, :character 0}, :end {:line 2, :character 3}, :text "baz"}])
  )
