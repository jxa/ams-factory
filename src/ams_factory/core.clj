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
  [stringish]
  (str (name stringish) "s"))

(defn singularize
  [stringish]
  (clojure.string/replace (name stringish) #"s$" ""))

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

(defn sideload
  "Include model-collection in payload in the manner of side-loading"
  ([payload model-collection]
     (sideload payload model-collection (-> model-collection
                                            first
                                            payload-root)))

  ([payload model-collection association-name]
     (let [model (extract-model payload)
           model-name (payload-root payload)
           associated-models (map extract-model model-collection)
           associated-model-name (-> model-collection
                                     first
                                     payload-root)
           associated-model-key (keyword (pluralize associated-model-name))
           existing-associated-models (get payload associated-model-key [])
           result (-> payload
                      (assoc-in [model-name (keyword (str (singularize association-name) "-ids"))]
                                (map :id associated-models))

                      (assoc associated-model-key
                        (uniq-by :id (concat existing-associated-models associated-models))))]
       (apply
        merge-with
        (fn [a b] (uniq-by :id (concat a b)))
        result
        (map sideloaded-models model-collection)))))


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

 (-> primary-model
      (sideload [creator] :collaborators)
      (sideload [(sideload author-task [some-guy] :assignee)])))
