{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Database.ORM.SelectSpec where

import Test.Hspec
import Control.Monad.State
import Control.Lens hiding ((:>))
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Proxy
import Data.Convertible
import Data.Extensible
import Data.Extensible.HList
import Data.Model.Graph
import Database.HDBC
import Database.ORM.Condition
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Query
import Database.ORM.Select
import Database.ORM.Utility
import Database.ORM.Dialect.Mock

type Extra1 = ExtraModel '["a" :> Int, "b" :> String] '[]
type Extra2 = ExtraModel '["c" :> Int, "d" :> String] '[]
type Extra3 = ExtraModel '["e" :> Int, "f" :> String] '[]
type Extra4 = ExtraModel '["g" :> Int, "h" :> String] '[]


type A = "a" :## Record '["aid" :> Int, "cola" :> String]
type B = "b" :## Record '["bid" :> Int, "colb" :> String]
type C = "c" :## Record '["cid" :> Int, "colc" :> String]
type D = "d" :## Record '["did" :> Int, "cold" :> String]
type E = "e" :## Record '["eid" :> Int, "cole" :> String]
type F = "f" :## Record '["fid" :> Int, "colf" :> String]

type R = "random" :## Record '["abc" :> String, "def" :> String, "ghi" :> String, "jkl" :> String]

type ABCDGraph = Graph A
                  :><: B
                  :><: C
                  :><: D
                  :><: (B :- A)
                  :><: (C :- B)
                  :><: (D :- B)

type WithExtraGraph = ABCDGraph
                 :><: Extra1
                 :><: Extra2
                 :><: (Extra1 :- B)
                 :><: (Extra2 :- Extra1)

type ABExtraGraph = Graph A
                     :><: B
                     :><: Extra1
                     :><: Extra2
                     :><: (B :- A)

type ABCD = '[A, B, C, D]
type WithExtra = '[A, B, C, D, Extra1, Extra2]
type ABExtra = '[A, B, Extra1, Extra2]

col :: Bool -> String -> Bool -> ColumnMeta
col pk n auto = ColumnMeta pk n "" False auto Nothing

rel :: ColumnMeta -> (String, String) -> ColumnMeta
rel cm (t, c) = cm { relation = Just (Relation t c) }

tableMap :: M.Map String TableMeta
tableMap = M.fromList [ ("a", TableMeta "a" [col True "aid" True, col False "cola" False])
                      , ("b", TableMeta "b" [col True "bid" True, col False "colb" False, col False "b_a_id" False `rel` ("a", "aid")])
                      , ("c", TableMeta "c" [col True "cid" True, col False "colc" False, col False "c_b_id" False `rel` ("b", "bid")])
                      , ("d", TableMeta "d" [col True "did" True, col False "cold" False, col False "d_b_id" False `rel` ("b", "bid")])
                      , ("e", TableMeta "e" [ col True "eid" True
                                            , col False "cole" False
                                            , col False "e_a_id" False `rel` ("a", "aid")
                                            , col False "e_c_id" False `rel` ("c", "cid")
                                            ])
                      , ("random", TableMeta "random" [ col False "abc" False
                                                      , col False "def" False
                                                      , col False "ghi" False
                                                      , col False "jkl" False
                                                      ])
                      ]

mock :: Mock
mock = Mock { dbUrl = ""
            , tables = tableMap
            } 

spec :: Spec
spec = do
    describe "Collect join informations" $ do
        it "Collect all" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                joins <- collectJoins (Proxy :: Proxy (CollectEdges ABCD (Edges ABCDGraph)))
                                      (Proxy :: Proxy ABCD)
                                      ["ta", "tb", "tc", "td"] :: IO [JoinEdge ABCDGraph ABCD]
                map show joins `shouldBe` [ "INNER JOIN b AS tb ON tb.b_a_id = ta.aid"
                                          , "INNER JOIN c AS tc ON tc.c_b_id = tb.bid"
                                          , "INNER JOIN d AS td ON td.d_b_id = tb.bid"
                                          ]

        it "Extra models don't generate join clause" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                joins <- collectJoins (Proxy :: Proxy (CollectEdges WithExtra (Edges WithExtraGraph)))
                                      (Proxy :: Proxy WithExtra)
                                      ["ta", "tb", "tc", "td"] :: IO [JoinEdge WithExtraGraph WithExtra]
                map show joins `shouldBe` [ "INNER JOIN b AS tb ON tb.b_a_id = ta.aid"
                                          , "INNER JOIN c AS tc ON tc.c_b_id = tb.bid"
                                          , "INNER JOIN d AS td ON td.d_b_id = tb.bid"
                                          , ""
                                          , ""
                                          ]

    describe "Select columns other than foreign keys" $ do
        it "Select columns" $ do
            let columns = selectColumns (Proxy :: Proxy '[A, B, C, D])
                                        (Proxy :: Proxy '[D, B, A, C])
                                        ["t1", "t2", "t3", "t4"]
            columns `shouldBe` [["t3.aid", "t3.cola"], ["t2.bid", "t2.colb"], ["t4.cid", "t4.colc"], ["t1.did", "t1.cold"]]

    describe "Obtain tables and their columns from graph" $ do
        it "All models" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                (columns, joins) <- columnsAndTables (Proxy :: Proxy ABCDGraph)
                                                     (Proxy :: Proxy A)
                                                     ["ta", "tb", "tc", "td"]
                columns `shouldBe` [ ["ta.aid", "ta.cola"]
                                   , ["tb.bid", "tb.colb", "tb.b_a_id"]
                                   , ["tc.cid", "tc.colc", "tc.c_b_id"]
                                   , ["td.did", "td.cold", "td.d_b_id"]
                                   ]
                map show joins `shouldBe` [ "INNER JOIN b AS tb ON tb.b_a_id = ta.aid"
                                          , "INNER JOIN c AS tc ON tc.c_b_id = tb.bid"
                                          , "INNER JOIN d AS td ON td.d_b_id = tb.bid"
                                          ]

        it "Contain model all of whose relation are not included in the graph" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                (columns, joins) <- columnsAndTables (Proxy :: Proxy (ABCDGraph :><: E :><: (E :- C)))
                                                     (Proxy :: Proxy A)
                                                     ["ta", "tb", "tc", "td", "te"]
                columns `shouldBe` [ ["ta.aid", "ta.cola"]
                                   , ["tb.bid", "tb.colb", "tb.b_a_id"]
                                   , ["tc.cid", "tc.colc", "tc.c_b_id"]
                                   , ["td.did", "td.cold", "td.d_b_id"]
                                   , ["te.eid", "te.cole", "te.e_c_id"]
                                   ]
                map show joins `shouldBe` [ "INNER JOIN b AS tb ON tb.b_a_id = ta.aid"
                                          , "INNER JOIN c AS tc ON tc.c_b_id = tb.bid"
                                          , "INNER JOIN d AS td ON td.d_b_id = tb.bid"
                                          , "INNER JOIN e AS te ON te.e_c_id = tc.cid"
                                          ]

        it "Model without relation to any traceable model has no effect" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                (columns, joins) <- columnsAndTables (Proxy :: Proxy (ABCDGraph :><: F))
                                                     (Proxy :: Proxy A)
                                                     ["ta", "tb", "tc", "td"]
                columns `shouldBe` [ ["ta.aid", "ta.cola"]
                                   , ["tb.bid", "tb.colb", "tb.b_a_id"]
                                   , ["tc.cid", "tc.colc", "tc.c_b_id"]
                                   , ["td.did", "td.cold", "td.d_b_id"]
                                   ]
                map show joins `shouldBe` [ "INNER JOIN b AS tb ON tb.b_a_id = ta.aid"
                                          , "INNER JOIN c AS tc ON tc.c_b_id = tb.bid"
                                          , "INNER JOIN d AS td ON td.d_b_id = tb.bid"
                                          ]

    describe "Convert row to a record" $ do
        it "New record" $ do
            (c, _) <- flip runStateT (newGraph :: ABCDGraph) $ do
                        (+<<) (Model (#aid @= 1 <: #cola @= "a1" <: emptyRecord) :: A)
                        rowToRecord (tableMap M.! "a") [("aid", toSql (2 :: Int)), ("cola", toSql "a2")] :: StateT ABCDGraph IO (Maybe (Cursor A))
            c `shouldSatisfy` isJust
            show (fromJust c) `shouldBe` "Cursor 1"

        it "Existing record" $ do
            (c, g) <- flip runStateT (newGraph :: ABCDGraph) $ do
                        (+<<) (Model (#aid @= 1 <: #cola @= "a1" <: emptyRecord) :: A)
                        rowToRecord (tableMap M.! "a") [("aid", toSql (1 :: Int)), ("cola", toSql "a2")] :: StateT ABCDGraph IO (Maybe (Cursor A))
            c `shouldSatisfy` isJust
            show (fromJust c) `shouldBe` "Cursor 0"
            let r = maybe (undefined :: A) (@< g) c
            view #aid (getRecord r) `shouldBe` (1 :: Int)
            view #cola (getRecord r) `shouldBe` "a1"

        it "Null record" $ do
            (c, g) <- flip runStateT (newGraph :: ABCDGraph) $ do
                        rowToRecord (tableMap M.! "a") [("aid", SqlNull), ("cola", SqlNull)] :: StateT ABCDGraph IO (Maybe (Cursor A))
            c `shouldSatisfy` isNothing
            length (values g :: [A]) `shouldBe` 0
                        
        it "Record whose fields are in random order" $ do
            (c, g) <- flip runStateT (newGraph :: Graph R) $ do
                        rowToRecord (tableMap M.! "random")
                                    [("ghi", toSql "GHI"), ("def", toSql "DEF"), ("jkl", toSql "JKL"), ("abc", toSql "ABC")]
                                        :: StateT (Graph R) IO (Maybe (Cursor R))
            c `shouldSatisfy` isJust
            let r = getRecord (fromJust c @< g) in do
                view #abc r `shouldBe` "ABC"
                view #def r `shouldBe` "DEF"
                view #ghi r `shouldBe` "GHI"
                view #jkl r `shouldBe` "JKL"

    describe "Parse and return cursor list" $ do
        it "New cursor" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                (cursors, g) <- flip runStateT (newGraph :: ABCDGraph) $ do
                                    parseRow (Proxy :: Proxy ABCD) [ [("aid", toSql (1 :: Int)), ("cola", toSql "aaa")]
                                                                   , [("bid", toSql (2 :: Int)), ("colb", toSql "bbb"), ("b_a_id", toSql (1 :: Int))]
                                                                   , [("cid", toSql (3 :: Int)), ("colc", toSql "ccc"), ("c_b_id", toSql (2 :: Int))]
                                                                   , [("did", toSql (4 :: Int)), ("cold", toSql "ddd"), ("d_b_id", toSql (2 :: Int))]
                                                                   ]
                let (ca, cs1) = headCursor cursors
                let (cb, cs2) = headCursor cs1
                let (cc, cs3) = headCursor cs2
                let (cd, _) = headCursor cs3
                let ra = getRecord (fromJust ca @< g) in do
                            view #aid ra `shouldBe` (1 :: Int)
                            view #cola ra `shouldBe` "aaa"
                let rb = getRecord (fromJust cb @< g) in do
                            view #bid rb `shouldBe` (2 :: Int)
                            view #colb rb `shouldBe` "bbb"
                let rc = getRecord (fromJust cc @< g) in do
                            view #cid rc `shouldBe` (3 :: Int)
                            view #colc rc `shouldBe` "ccc"
                let rd = getRecord (fromJust cd @< g) in do
                            view #did rd `shouldBe` (4 :: Int)
                            view #cold rd `shouldBe` "ddd"

        it "Extras exist" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                (cursors, g) <- flip runStateT (newGraph :: ABExtraGraph) $ do
                                    parseRow (Proxy :: Proxy ABExtra) [ [("aid", toSql (1 :: Int)), ("cola", toSql "aaa")]
                                                                      , [("bid", toSql (2 :: Int)), ("colb", toSql "bbb"), ("b_a_id", toSql (1 :: Int))]
                                                                      , [("a", toSql (3 :: Int)), ("b", toSql "ccc")]
                                                                      , [("c", toSql (4 :: Int)), ("d", toSql "ddd")]
                                                                      ]
                let (ca, cs1) = headCursor cursors
                let (cb, cs2) = headCursor cs1
                let (cc, cs3) = headCursor cs2
                let (cd, _) = headCursor cs3
                let ra = getRecord (fromJust ca @< g) in do
                            view #aid ra `shouldBe` (1 :: Int)
                            view #cola ra `shouldBe` "aaa"
                let rb = getRecord (fromJust cb @< g) in do
                            view #bid rb `shouldBe` (2 :: Int)
                            view #colb rb `shouldBe` "bbb"
                let rc = getRecord (fromJust cc @< g) in do
                            view #a rc `shouldBe` (3 :: Int)
                            view #b rc `shouldBe` "ccc"
                let rd = getRecord (fromJust cd @< g) in do
                            view #c rd `shouldBe` (4 :: Int)
                            view #d rd `shouldBe` "ddd"

        it "Contain null" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                (cursors, g) <- flip runStateT (newGraph :: ABCDGraph) $ do
                                    parseRow (Proxy :: Proxy '[A, B]) [ [("aid", toSql (1 :: Int)), ("cola", toSql "aaa")]
                                                                      , [("bid", SqlNull), ("colb", SqlNull), ("b_a_id", SqlNull)]
                                                                      ]
                let (ca, cs1) = headCursor cursors
                let (cb, cs2) = headCursor cs1
                let ra = getRecord (fromJust ca @< g) in do
                            view #aid ra `shouldBe` (1 :: Int)
                            view #cola ra `shouldBe` "aaa"
                cb `shouldSatisfy` isNothing

    describe "Add an edge to the graph" $ do
        it "Edges between existing cursors" $ do
            let mc c = MaybeCursor (Just c)

            (_, g') <- flip runStateT (newGraph :: ABCDGraph) $ do
                        a1 <- (+<<) (Model (#aid @= 1 <: #cola @= "a1" <: emptyRecord) :: A)
                        b1 <- (+<<) (Model (#bid @= 1 <: #colb @= "b1" <: emptyRecord) :: B)
                        c1 <- (+<<) (Model (#cid @= 1 <: #colc @= "c1" <: emptyRecord) :: C)
                        d1 <- (+<<) (Model (#did @= 1 <: #cold @= "d1" <: emptyRecord) :: D)

                        let cursors = mc a1 `HCons` mc b1 `HCons` mc c1 `HCons` mc d1 `HCons` HNil
                        addEdge cursors (JoinEdge (Proxy :: Proxy B) (Proxy :: Proxy A) (Proxy :: Proxy '[]) Nothing)
                        addEdge cursors (JoinEdge (Proxy :: Proxy C) (Proxy :: Proxy B) (Proxy :: Proxy '[]) Nothing)

            length (values g' :: [B :- A]) `shouldBe` 1
            length (values g' :: [C :- B]) `shouldBe` 1
            length (values g' :: [D :- B]) `shouldBe` 0

        it "Edge associated with missing cursor" $ do
            let mc c = MaybeCursor (Just c)

            (_, g') <- flip runStateT (newGraph :: ABCDGraph) $ do
                        a1 <- (+<<) (Model (#aid @= 1 <: #cola @= "a1" <: emptyRecord) :: A)
                        b1 <- (+<<) (Model (#bid @= 1 <: #colb @= "b1" <: emptyRecord) :: B)
                        d1 <- (+<<) (Model (#did @= 1 <: #cold @= "d1" <: emptyRecord) :: D)

                        let cursors = mc a1 `HCons` mc b1 `HCons` MaybeCursor Nothing `HCons` mc d1 `HCons` HNil
                        addEdge cursors (JoinEdge (Proxy :: Proxy B) (Proxy :: Proxy A) (Proxy :: Proxy '[]) Nothing)
                        addEdge cursors (JoinEdge (Proxy :: Proxy C) (Proxy :: Proxy B) (Proxy :: Proxy '[]) Nothing)

            length (values g' :: [B :- A]) `shouldBe` 1
            length (values g' :: [C :- B]) `shouldBe` 0

    describe "Query creation" $ do
        it "With all kinds of clauses" $ do
            let c = cond @'[A] "#" "#.cola = ?" .& cond @'[C, D] "#" "#.cid = #.did * ?" .+ "aaa" .+ (2 :: Int)
            let o = orderBy @B "bid" ASC ./ orderBy @D "did" DESC

            q <- queryWithComponents c o (Just (5, 10))

            q `shouldBe` "SELECT \
                        \t0.aid, t0.cola, t1.bid, t1.colb, t1.b_a_id, t2.cid, t2.colc, t2.c_b_id, t3.did, t3.cold, t3.d_b_id \
                        \FROM a AS t0 \
                        \INNER JOIN b AS t1 ON t1.b_a_id = t0.aid \
                        \INNER JOIN c AS t2 ON t2.c_b_id = t1.bid \
                        \INNER JOIN d AS t3 ON t3.d_b_id = t1.bid \
                        \WHERE (t0.cola = ?) AND (t2.cid = t3.did * ?) \
                        \ORDER BY t1.bid ASC, t3.did DESC LIMIT ? OFFSET ?\
                        \"

        it "No conditions" $ do
            let o = orderBy @B "bid" ASC ./ orderBy @D "did" DESC

            q <- queryWithComponents (..?) o (Just (5, 10))

            q `shouldBe` "SELECT \
                        \t0.aid, t0.cola, t1.bid, t1.colb, t1.b_a_id, t2.cid, t2.colc, t2.c_b_id, t3.did, t3.cold, t3.d_b_id \
                        \FROM a AS t0 \
                        \INNER JOIN b AS t1 ON t1.b_a_id = t0.aid \
                        \INNER JOIN c AS t2 ON t2.c_b_id = t1.bid \
                        \INNER JOIN d AS t3 ON t3.d_b_id = t1.bid \
                        \ORDER BY t1.bid ASC, t3.did DESC LIMIT ? OFFSET ?\
                        \"

        it "No orders" $ do
            let c = cond @'[A] "#" "#.cola = ?" .& cond @'[C, D] "#" "#.cid = #.did * ?" .+ "aaa" .+ (2 :: Int)

            q <- queryWithComponents c (../) (Just (5, 10))

            q `shouldBe` "SELECT \
                        \t0.aid, t0.cola, t1.bid, t1.colb, t1.b_a_id, t2.cid, t2.colc, t2.c_b_id, t3.did, t3.cold, t3.d_b_id \
                        \FROM a AS t0 \
                        \INNER JOIN b AS t1 ON t1.b_a_id = t0.aid \
                        \INNER JOIN c AS t2 ON t2.c_b_id = t1.bid \
                        \INNER JOIN d AS t3 ON t3.d_b_id = t1.bid \
                        \WHERE (t0.cola = ?) AND (t2.cid = t3.did * ?) \
                        \LIMIT ? OFFSET ?\
                        \"

        it "No limit and offset" $ do
            let c = cond @'[A] "#" "#.cola = ?" .& cond @'[C, D] "#" "#.cid = #.did * ?" .+ "aaa" .+ (2 :: Int)
            let o = orderBy @B "bid" ASC ./ orderBy @D "did" DESC

            q <- queryWithComponents c o Nothing

            q `shouldBe` "SELECT \
                        \t0.aid, t0.cola, t1.bid, t1.colb, t1.b_a_id, t2.cid, t2.colc, t2.c_b_id, t3.did, t3.cold, t3.d_b_id \
                        \FROM a AS t0 \
                        \INNER JOIN b AS t1 ON t1.b_a_id = t0.aid \
                        \INNER JOIN c AS t2 ON t2.c_b_id = t1.bid \
                        \INNER JOIN d AS t3 ON t3.d_b_id = t1.bid \
                        \WHERE (t0.cola = ?) AND (t2.cid = t3.did * ?) \
                        \ORDER BY t1.bid ASC, t3.did DESC\
                        \"

headCursor :: HList MaybeCursor (a ': as)
           -> (Maybe (Cursor a), HList MaybeCursor as)
headCursor (c `HCons` cs) = (getCursor c, cs)

queryWithComponents :: (ElemIndexes ts ABCD, ElemIndexes us ABCD)
                    => Condition (ts :: [*])
                    -> OrderBy (us :: [*])
                    -> LimitOffset
                    -> IO String
queryWithComponents c o lo = do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                let aliases = ["t0", "t1", "t2", "t3"]

                (columns, joins) <- columnsAndTables (Proxy :: Proxy ABCDGraph) (Proxy :: Proxy A) aliases

                let q = createSelectQuery columns ("a", "t0") joins
                                            (formatCondition c (Proxy :: Proxy '[A, B, C, D]) aliases)
                                            (formatOrderBy o (Proxy :: Proxy '[A, B, C, D]) aliases)
                                            lo

                return q