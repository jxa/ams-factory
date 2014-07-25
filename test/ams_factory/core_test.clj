(ns ams-factory.core-test
  (:require [clojure.test :refer :all]
            [ams-factory.core :refer :all]))

(deftest test-payload
  (let [model {:id 4 :name "thing"}]
    (testing "payload"
      (is (= model (extract-model (->payload :thing model)))))))

(deftest test-sideload
  (let [primary-model (->payload :paper
                               {:id 2
                                :title "On the Origin of Species"})
        author-task {:id 5
                     :title "Assign Author"
                     :kind "AssignAuthorTask"}]

    (testing "including the associated collection in the root"
      (is (= [author-task]
             (:tasks (sideload primary-model [(->payload :task author-task)])))))

    (testing "including the ids in the primary model"
      (is (= [5]
             (-> (sideload primary-model [(->payload :task author-task)])
                 :paper
                 :task-ids))))

    (testing "overriding the association name"
      (let [payload (sideload primary-model [(->payload :task author-task)] :todos)]
        (is (= [5]
               (-> payload
                   :paper
                   :todo-ids)))
        (is (= [author-task]
               (-> payload
                   :tasks)))))))

(deftest test-sideload-merge
  (testing "merging two payloads which each have sideloads"
    (let [primary-model (->payload :paper
                                   {:id 2
                                    :title "On the Origin of Species"})
          author-task (->payload :task
                                 {:id 5
                                  :title "Assign Author"
                                  :kind "AssignAuthorTask"})
          some-guy (->payload :user
                              {:id 9
                               :name "Chris"})
          creator (->payload :user
                              {:id 3
                               :name "Steve"})]
      (is (= #{(extract-model creator) (extract-model some-guy)}
             (-> primary-model
                 (sideload [creator] :collaborators)
                 (sideload [(sideload author-task [some-guy] :assignee)])
                 :users
                 set)))

      (testing "uniqueness of merged payloads"
        (is (= #{(extract-model creator) (extract-model some-guy)}
               (-> primary-model
                   (sideload [creator some-guy] :collaborators)
                   (sideload [(sideload author-task [some-guy] :assignee)])
                   :users
                   set)))))))
