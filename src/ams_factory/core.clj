(ns ams-factory.core
  (:require [clojure.tools.trace :refer (deftrace trace)]))

(defn ->payload
  [name m]
  {(keyword name) m
   :__root__ (keyword name)})

(defn payload-root
  [payload]
  (:__root__ payload))

(defn extract-model
  [payload]
  (get payload (payload-root payload)))

(defn pluralize
  [s]
  (str (name s) "s"))

(defn singularize
  [s]
  (clojure.string/replace (name s) #"s$" ""))

(defn uniq-by
  [f c]
  (letfn [(uniq-inner [seen items]
            (lazy-seq
             (if (seq items)
               (let [items (drop-while #(contains? seen (f %)) items)
                     item (first items)]
                 (when item
                   (cons item (uniq-inner (conj seen (f item))
                                          (rest items))))))))]
    (uniq-inner #{} c)))

(defn sideloaded-models
  [payload]
  (dissoc payload
          (payload-root payload)
          :__root__))

(defn model-collection-name
  [model-collection]
  (-> model-collection
      first
      payload-root))

(defn merge-sideload
  [payload key models]
  (assoc payload key
         (uniq-by :id
                  (concat (get payload key [])
                          models))))

(defn merge-sideloads
  [payload sideloads]
  (reduce (fn [final-payload [key model]]
            (merge-sideload final-payload key model))
          payload
          sideloads))

(defn merge-sideloaded-models
  [payload models]
  (reduce (fn [final-payload model]
            (merge-sideloads final-payload (sideloaded-models model)))
          payload
          models))

;; users: [] <--- users is the collection-key
;; user_ids: [] <--- user_ids is the foreign-key
(defn sideload
  [payload collection-key foreign-key foreign-val collection]
  (let [model-name (payload-root payload)
        associated-models (map extract-model collection)]
    (-> payload
        (assoc-in [model-name foreign-key] foreign-val)
        (merge-sideload collection-key associated-models)
        (merge-sideloaded-models collection))))

(defn sideload-many
  "Include model-collection in payload in the manner of side-loading"
  ([payload model-collection]
     (sideload-many payload model-collection (model-collection-name model-collection)))

  ([payload model-collection association-name]
     (sideload
      payload
      (keyword (pluralize (model-collection-name model-collection)))
      (keyword (str (singularize association-name) "-ids"))
      (map (comp :id extract-model) model-collection)
      model-collection)))


(defn sideload-one
  "TODO: ensure payload and model are PAYLOADs"
  ([payload model]
     (sideload-one payload model (payload-root model)))

  ([payload model association-name]
     (sideload
      payload
      (keyword (pluralize (payload-root model)))
      (keyword (str (singularize association-name) "-id"))
      (:id (extract-model model))
      [model])))

(comment
 (def primary-model (->payload :paper
                               {:id 2
                                :title "On the Origin of Species"}))

 (def author-task (->payload :task
                             {:id 5
                              :title "Assign Author"
                              :kind "AssignAuthorTask"}))

 (def some-guy (->payload :user
                          {:id 9
                           :name "Chris"}))

 (def creator (->payload :user
                         {:id 3
                          :name "Steve"}))

 (sideload-many primary-model [creator])
 (sideload-one primary-model creator)
 (sideload-one primary-model
               (sideload-one some-guy creator :father))

 (-> primary-model
      (sideload-many [creator] :collaborators)
      (sideload-many [(sideload-many author-task [some-guy] :assignee)])))
