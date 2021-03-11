(ns hi55.tictactoe-test
  (:require [clojure.test :refer :all]
            [hi55.tictactoe :refer :all]))

(deftest test-game
  (testing "computer picks empty corner"
    (is (= {:board [[:e :e :e] 
                    [:e :o :e] 
                    [:e :e :x]] :state :ongoing}
           (game [[:e :e :e]
                  [:e :e :e]
                  [:e :e :e]] 1 1))))
  (testing "computer picks opposite corner"
    (is (= {:board [[:x :e :e] 
                    [:e :x :e] 
                    [:e :e :o]] :state :ongoing}
           (game [[:e :e :e]
                  [:e :x :e]
                  [:e :e :e]] 2 2))))
  
  (testing "computer picks center"
    (is (= {:board [[:o :e :e] 
                    [:e :x :e] 
                    [:e :e :e]] :state :ongoing}
           (game [[:e :e :e]
                  [:e :e :e]
                  [:e :e :e]] 0 0))))

  (testing "computer blocks fork"
    (is (= {:board [[:o :e :e]
                    [:x :x :e]
                    [:e :o :e]]
            :state :ongoing}
           (game
            [[:o :e :e]
             [:x :e :e]
             [:e :e :e]] 1 2))))
  (testing "computer picks fork"
    (is (= {:board [[:x :e :x]
                    [:o :e :x]
                    [:o :e :e]]
            :state :ongoing}
           (game [[:x :e :e]
                  [:o :e :x]
                  [:e :e :e]] 0 2))))
  (testing "computer picks blocking move"
    (is (= {:board [[:x :e :e]
                    [:e :e :e]
                    [:o :x :o]]
            :state :ongoing}
           (game [[:x :e :e]
                  [:e :e :e]
                  [:o :e :e]] 2 2)))
    (is (= {:board [[:x :e :o]
                    [:e :x :e]
                    [:o :e :e]]
            :state :ongoing}
           (game [[:x :e :e]
                  [:e :e :e]
                  [:o :e :e]] 2 0))))
  

  (testing "computer picks winning move"
    (is (= {:board [[:x :x :x]
                    [:e :o :o]
                    [:o :o :e]]
            :state :x}
           (game [[:x :e :x]
                  [:e :e :o]
                  [:o :o :e]] 1 1)))
    
    (is (= {:board [[:x :e :o]
                    [:e :x :o]
                    [:o :o :x]]
            :state :x}
           (game [[:x :e :e]
                  [:e :e :o]
                  [:o :o :x]] 2 0)))
    
    (is (= {:board [[:x :e :o]
                    [:x :e :o]
                    [:x :o :x]]
            :state :x}
           (game [[:x :e :e]
                  [:e :e :o]
                  [:x :o :x]] 2 0)))))

(deftest test-next-moves
  (testing "next moves"
    (is (= [[[:x :x :x]
             [:e :o :o]
             [:x :o :x]]
            [[:x :e :x]
             [:x :o :o]
             [:x :o :x]]]
           (next-moves [[:x :e :x]
                        [:e :o :o]
                        [:x :o :x]] :x)))))

(deftest test-analyse
  (testing "ongoing"
    (is (= :ongoing (analyse
                     [[:x :e :x]
                      [:e :e :e]
                      [:o :e :o]]))))
  (testing "draw"
    (is (= :draw (analyse
                     [[:x :o :x]
                      [:o :o :x]
                      [:o :x :o]]))))
  (testing "X should win"
    (is (= :x (analyse [[:x :x :x]
                        [:e :e :e]
                        [:o :o :o]])))
    (is (= :x (analyse [[:x :o :x]
                        [:e :x :e]
                        [:o :o :x]])))
    (is (= :x (analyse [[:o :x :x]
                        [:e :x :e]
                        [:x :o :o]])))
    (is (= :x (analyse [[:x :o :x]
                        [:x :x :o]
                        [:x :o :x]]))))
  (testing "O should win"
    (is (= :o (analyse [[:x :x :e]
                        [:e :e :e]
                        [:o :o :o]])))
    (is (= :o (analyse [[:x :o :o]
                        [:e :o :e]
                        [:o :x :x]])))
    (is (= :o (analyse [[:o :x :x]
                        [:e :o :e]
                        [:x :o :o]])))
    (is (= :o (analyse [[:x :o :x]
                        [:e :o :o]
                        [:x :o :x]])))))
